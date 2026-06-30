# Rwards

The `Rwards` package turns R errors into opportunities for growth and engagement. Instead of dreading error messages, you earn **XP** for every error you hit and you get a bonus the first time you encounter a new exception. As your XP grows you climb **levels with playful titles**, with a **progress bar** toward the next level and an encouraging message on every error. Your progress is **saved across R sessions**, so the goal is simple: take the anxiety out of mistakes and make learning R supportive and fun for newcomers.


## Getting Started


To develop without having to install:
```r
devtools::load_all()

# Trigger any error and watch your XP grow:
sqrt("a")
```

To install from GitHub:
```r
devtools::install_github("mickaeltemporao/exceptional-Rwards")
library(Rwards)

sqrt("a")
```

Prefer a cyberpunk flavour of level titles? Set this option (before or after loading):
```r
options(Rwards.theme = "cyberpunk")
```

## AI Acknowledgement

The development of some features and tests in this package were accelerated with the assistance of an AI coding assistant.

