# XP Thresholds based on Diablo 2 curve from levels.md
xp_thresholds <- c(
  500,     # Level 1
  1625,    # Level 2
  3875,    # Level 3
  8875,    # Level 4
  19025,   # Level 5
  33650,   # Level 6
  52775,   # Level 7
  75275,   # Level 8
  109025,  # Level 9
  154025   # Level 10
)

# Level Titles Themes
level_themes <- list(
  basic = c(
    "Novice", "Explorer", "Tinkerer", "Problem Solver", "Builder", 
    "Debugger", "Strategist", "Hacker", "Architect", "Mastermind"
  ),
  cyberpunk = c(
    "Wired Rookie", "Neon Scout", "Script Tinkerer", "Console Runner", 
    "Code Slinger", "Signal Cracker", "Firewall Buster", "Ghost in the Net", 
    "System Phantom", "The Rootwalker"
  )
)

# Random positive messages
positive_messages <- c(
  "Every mistake is a stepping stone to mastery!",
  "Another error? Just another puzzle piece falling into place.",
  "You're not failing, you're learning what doesn't work.",
  "Debugging is like being a detective in a crime movie where you are also the murderer.",
  "Great job! Now you know one more way NOT to do it.",
  "Oops! But hey, that's how we learn.",
  "Keep at it! Even the best developers see this all the time.",
  "Don't worry, even R gets confused sometimes.",
  "Embrace the red text! It's just R's way of talking to you.",
  "You're doing awesome. A little bug can't stop you!"
)

get_level_info <- function(xp) {
  level <- sum(xp >= xp_thresholds) + 1
  if (level > length(xp_thresholds)) {
    level <- length(xp_thresholds) # Max level cap
  }
  
  theme_name <- getOption("Rwards.theme", default = "basic")
  if (!theme_name %in% names(level_themes)) {
    theme_name <- "basic"
  }
  
  title <- level_themes[[theme_name]][level]
  
  if (level == 1) {
    prev_threshold <- 0
    next_threshold <- xp_thresholds[1]
  } else if (level == length(xp_thresholds)) {
    prev_threshold <- xp_thresholds[level - 1]
    next_threshold <- xp_thresholds[level] # Stay at max
  } else {
    prev_threshold <- xp_thresholds[level - 1]
    next_threshold <- xp_thresholds[level]
  }
  
  list(
    level = level,
    title = title,
    next_threshold = next_threshold,
    prev_threshold = prev_threshold
  )
}
