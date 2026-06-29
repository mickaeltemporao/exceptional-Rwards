test_that("Level XP and titles are calculated correctly", {
  # Level 1
  info <- get_level_info(0)
  expect_equal(info$level, 1)
  expect_equal(info$title, "Novice")
  
  # Level 2 threshold
  info2 <- get_level_info(500)
  expect_equal(info2$level, 2)
  expect_equal(info2$title, "Explorer")
  
  # Max Level
  info_max <- get_level_info(200000)
  expect_equal(info_max$level, 10)
  expect_equal(info_max$title, "Mastermind")
  
  # Cyberpunk theme
  options(Rwards.theme = "cyberpunk")
  info_cyber <- get_level_info(500)
  expect_equal(info_cyber$title, "Neon Scout")
  
  # Fallback to basic if invalid theme
  options(Rwards.theme = "invalid_theme")
  info_fallback <- get_level_info(500)
  expect_equal(info_fallback$title, "Explorer")
  
  # Reset options
  options(Rwards.theme = "basic")
})

test_that("XP tracking increments correctly", {
  old_points <- error_tracker$points
  old_types <- error_tracker$error_types
  
  error_tracker$error_types <- list()
  error_tracker$points <- 0
  
  # Simulate first new error
  e1 <- simpleError("First test error")
  
  # Use suppressMessages because error_handler calls message()
  suppressMessages(error_handler(e1))
  expect_equal(error_tracker$points, 150)
  
  # Simulate same error again
  suppressMessages(error_handler(e1))
  expect_equal(error_tracker$points, 200) # 150 + 50
  
  # Restore
  error_tracker$error_types <- old_types
  error_tracker$points <- old_points
})
