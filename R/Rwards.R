# Function to handle errors
error_handler <- function(e) {
  # Error message
  error_message <- conditionMessage(e)
  
  # Check if it's a new type of error (without modifying the original error message)
  if (!(error_message %in% names(error_tracker$error_types))) {
    error_tracker$error_types[[error_message]] <- TRUE  # Track new error type
    error_tracker$points <- error_tracker$points + 3     # Add 3 points for a new error type
  } else {
    error_tracker$points <- error_tracker$points + 1     # Add 1 point for a recurring error
  }

  # Assign the color based on points
  color_func <- if (error_tracker$points >= 500) {
    crayon::red
  } else if (error_tracker$points >= 250) {
    crayon::magenta
  } else if (error_tracker$points >= 100) {
    crayon::blue
  } else if (error_tracker$points >= 50) {
    crayon::green
  } else if (error_tracker$points >= 20) {
    crayon::yellow
  } else {
    crayon::silver
  }

  # Create a positive message
  message <- if (error_tracker$points >= 500) {
    "You're doing a fantastic job! Way to go!"
  } else if (error_tracker$points >= 250) {
    "You're getting better with each step!"
  } else if (error_tracker$points >= 100) {
    "Nice work! You'll get this!"
  } else if (error_tracker$points >= 50) {
    "Keep going, every mistake is progress!"
  } else if (error_tracker$points >= 20) {
    "You're doing awesome!"
  } else if (error_tracker$points >= 4) {
    "More mistakes will keep earning you more points!"
  } else {
    paste("This is your first mistake. Humans learn by making mistakes and",
          "fixing them. Keep up the good work and you will accumulate points!")
  }
  
  # Print the message in the selected color
  message(color_func(paste("Error detected:", error_message)))
  message(color_func(paste("Points:", error_tracker$points)))
  message(color_func(message))
  
  # Return the original error for normal propagation
  return(e)
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
error_tracker$error_types <<- list()  # To track unique error messages
error_tracker$points <<- 0            # Total points

# Set custom error/warning handlers
options(error = evaluate_code)
message("Rwards ready to reward!")
}
