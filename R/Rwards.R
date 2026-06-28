# Cache functions
get_cache_dir <- function() {
  tools::R_user_dir("Rwards", which = "data")
}

get_cache_file <- function() {
  file.path(get_cache_dir(), "progress.rds")
}

load_progress <- function() {
  cache_file <- get_cache_file()
  if (file.exists(cache_file)) {
    tryCatch({
      data <- readRDS(cache_file)
      error_tracker$error_types <- data$error_types
      error_tracker$points <- data$points
      # Backwards compatibility check
      if (is.null(error_tracker$points)) error_tracker$points <- 0
      if (is.null(error_tracker$error_types)) error_tracker$error_types <- list()
    }, error = function(e) {
      # On failure to read, just start fresh
      error_tracker$error_types <- list()
      error_tracker$points <- 0
    })
  } else {
    error_tracker$error_types <- list()
    error_tracker$points <- 0
  }
}

save_progress <- function() {
  cache_dir <- get_cache_dir()
  if (!dir.exists(cache_dir)) {
    dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
  }
  data <- list(
    error_types = error_tracker$error_types,
    points = error_tracker$points
  )
  saveRDS(data, file = get_cache_file())
}

# Function to handle errors
error_handler <- function(e) {
  # Error message
  error_message <- conditionMessage(e)
  
  # Current level info before points added
  info_before <- get_level_info(error_tracker$points)
  
  # Check if it's a new type of error
  if (!(error_message %in% names(error_tracker$error_types))) {
    error_tracker$error_types[[error_message]] <- TRUE
    error_tracker$points <- error_tracker$points + 150
  } else {
    error_tracker$points <- error_tracker$points + 50
  }
  
  # Save to cache
  save_progress()
  
  # Current level info after points added
  info_after <- get_level_info(error_tracker$points)
  
  # Initialize or Update progress bar if level changed or bar not present
  if (is.null(error_tracker$progress) || info_before$level != info_after$level) {
    initialize_progress_bar(info_after)
  }
  
  update_progress_bar(info_after)
  
  # Print the error details first
  message(crayon::red(paste("\nError detected:", error_message)))
  
  # Level up announcement or positive message
  if (info_after$level > info_before$level) {
    message(crayon::bold(crayon::yellow(
      sprintf("\n\u2B50 LEVEL UP! \u2B50 You reached Level %d: %s!", info_after$level, info_after$title)
    )))
  } else {
    # Random positive message
    msg <- sample(positive_messages, 1)
    message(crayon::green(sprintf("\n%s", msg)))
  }
  
  message(crayon::cyan(sprintf("Total XP: %d", error_tracker$points)))
  
  # Return the original error for normal propagation
  return(e)
}

# Initialize the progress bar
initialize_progress_bar <- function(info) {
  total_xp_for_level <- info$next_threshold - info$prev_threshold
  if (total_xp_for_level <= 0) total_xp_for_level <- 1 # safety
  
  error_tracker$progress <- progress::progress_bar$new(
    format = sprintf("[Level %d: %s] Progress to next level: [:bar] :percent XP: :current_xp/:total_xp", info$level, info$title),
    total = total_xp_for_level,
    clear = FALSE, width = 80
  )
}

# Update progress bar
update_progress_bar <- function(info) {
  current_xp_in_level <- error_tracker$points - info$prev_threshold
  total_xp_for_level <- info$next_threshold - info$prev_threshold
  if (total_xp_for_level <= 0) total_xp_for_level <- 1
  
  # Cap progress at total to prevent errors
  if (current_xp_in_level > total_xp_for_level) {
    current_xp_in_level <- total_xp_for_level
  }
  
  error_tracker$progress$update(
    ratio = current_xp_in_level / total_xp_for_level,
    tokens = list(
      current_xp = current_xp_in_level,
      total_xp = total_xp_for_level
    )
  )
}

# Function to evaluate code and catch errors
evaluate_code <- function(expr) {
  tryCatch(
    eval(expr),
    error = error_handler
  )
}

.onLoad <- function(libname, pkgname) {
  # Create global environment to store error types and points
  error_tracker <<- new.env()
  
  # Load saved progress
  load_progress()
  
  # Initialize progress bar
  info <- get_level_info(error_tracker$points)
  initialize_progress_bar(info)
  
  # Set custom error handler that doesn't require eval
  options(error = function() {
    # Capture the last error
    e <- geterrmessage()
    # Call the error handler with the captured error
    error_handler(simpleError(e))
  })
  
  message("Rwards ready to reward! Set 'options(Rwards.theme = \"cyberpunk\")' for a different flavor.")
}
