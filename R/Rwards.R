error_tracker <- new.env(parent = emptyenv())

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
# `reprint`: when TRUE the handler prints the error itself. This is only needed
# for the evaluate_code() path, where tryCatch swallows the error and R never
# prints it. In the options(error=) path R has already printed the native error
# before this runs, so reprinting would show every error twice.
error_handler <- function(e, reprint = FALSE) {
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
  
  # Print the error details first (only when R hasn't already done so)
  if (reprint) {
    message(crayon::red(paste("\nError detected:", error_message)))
  }
  
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
    format = sprintf("[Level %d: %s] Progress to next level: [:bar] :percent XP: :xp_current/:xp_total", info$level, info$title),
    total = total_xp_for_level,
    clear = FALSE, width = 80,
    show_after = 0, force = TRUE
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
      xp_current = current_xp_in_level,
      xp_total = total_xp_for_level
    )
  )
}

#' Evaluate Code and Reward Errors
#'
#' Evaluate an R expression, turning any error it raises into experience
#' points. This is a legacy helper: once the package is attached with
#' `library(Rwards)` the installed error handler rewards errors
#' automatically, so `evaluate_code()` is rarely needed.
#'
#' @param expr An unevaluated R expression (for example one produced by
#'   [quote()]) to evaluate.
#'
#' @return The value of `expr`. If evaluating `expr` raises an error, the
#'   error is turned into experience points (with a level / progress-bar
#'   update) and the captured error condition is returned invisibly instead
#'   of being signalled.
#'
#' @examples
#' # A successful expression is returned as usual
#' evaluate_code(quote(1 + 1))
#'
#' @export
evaluate_code <- function(expr) {
  tryCatch(
    eval(expr),
    error = function(e) error_handler(e, reprint = TRUE)
  )
}

# Activation happens in .onAttach (i.e. when the user runs library(Rwards)), not
# .onLoad, so merely importing the namespace never touches global session state.
# The user's previous error handler is saved here and restored in .onDetach, so
# Rwards leaves the global state exactly as it found it -- which is what CRAN
# requires of packages that modify session-wide settings.
.onAttach <- function(libname, pkgname) {
  # Load saved progress and prepare the progress bar
  load_progress()
  initialize_progress_bar(get_level_info(error_tracker$points))

  # Install our error handler, remembering the user's previous one
  error_tracker$prev_error_option <- getOption("error")
  options(error = function() {
    # Capture the last error and hand it to the gamification layer
    error_handler(simpleError(geterrmessage()))
  })

  packageStartupMessage(
    "Rwards ready to reward! Set 'options(Rwards.theme = \"cyberpunk\")' for a different flavor."
  )
}

.onDetach <- function(libpath) {
  # Restore the error handler that was in place before Rwards was attached
  options(error = error_tracker$prev_error_option)
}
