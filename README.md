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

## Contributing

Contributions are very welcome! Whether it's adding new features, expanding translations, or designing new themes, we'd love your help to make learning R more fun. Please see our [Contributing Guidelines](CONTRIBUTING.md) for more details.

## AI Acknowledgement

The development of some features and tests in this package were accelerated with the assistance of an AI coding assistant.

<!-- badges: start -->
  [![R-CMD-check](https://github.com/mickaeltemporao/exceptional-Rwards/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/mickaeltemporao/exceptional-Rwards/actions/workflows/R-CMD-check.yaml)
  <!-- badges: end -->

## TODO
- [x] Remove need to use evaluate_code
- [x] Change error_tracker to progress tracker or exp (stay positive)
- [x] Add a sense of progression/level-up as a gratification mechanism (eg. novice, amateur, ..., bugmaster)
- [x] Keep track of points between sessions? sort of a cache?
- [x] Show progress until next level?
- [x] Contributor Guidelines
- [ ] pride_colour color palette: new color when new error discovered
- [ ] Add language (en/fr/de/...) option
- [ ] Remove redundant text in error message.
- [ ] Put .onLoad inside zzz.R
- [ ] Minimal doc
- [ ] mvp for cran sub
