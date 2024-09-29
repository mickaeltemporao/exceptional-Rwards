# Load the necessary package
if (!requireNamespace("crayon", quietly = TRUE)) {
  install.packages("crayon")
}
library(crayon)

# Create an environment to store error types and points
error_tracker <- new.env()
error_tracker$error_types <- list()  # To track unique error messages
error_tracker$points <- 0            # Total points

# Function to handle errors
error_handler <- function(e) {
  # Error message
  error_message <- conditionMessage(e)
  
  # If it's a new type of error, add 3 points
  if (!(error_message %in% error_tracker$error_types)) {
    error_tracker$error_types[[error_message]] <- TRUE
    error_tracker$points <- error_tracker$points + 3
  }

  # Assign the color based on points
  color_func <- if (error_tracker$points >= 500) {
    red
  } else if (error_tracker$points >= 300) {
    magenta
  } else if (error_tracker$points >= 100) {
    blue
  } else if (error_tracker$points >= 50) {
    green
  } else if (error_tracker$points >= 20) {
    yellow
  } else {
    silver
  }

  # Create a positive message
  positive_messages <- c(
    "Great learning opportunity!",
    "You're doing awesome!",
    "Keep going, every mistake is progress!",
    "Nice work! You'll get this!",
    "You're getting better with each step!"
  )
  
  message <- sample(positive_messages, 1)
  
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

# Example of usage:
# You can wrap your code within evaluate_code like this:
# evaluate_code(quote(log("a")))  # This will trigger an error
evaluate_code(quote(log("a")))
