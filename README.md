# Rwards

The `Rwards` package is designed to transform the experience of learning R by turning errors into opportunities for growth and engagement. By rewarding users for making mistakes, this package aims to reduce the anxiety often associated with error messages and fosters a positive learning environment for newcomers. Users earn points or badges for each error encountered, encouraging exploration and experimentation without fear of failure. With built-in tips and helpful resources for troubleshooting common issues, `Rwards` empowers users to learn from their mistakes in a supportive and fun way.

## TODO
- [x] Remove need to use evaluate_code
- [x] Change error_tracker to progress tracker or exp (stay positive)
- [ ] pride_colour color palette: new color when new error discovered
- [ ] Add language (en/fr/de/...) option
- [ ] Add a sense of progression/level-up as a gratification mechanism (eg. novice, amateur, ..., bugmaster)
- [ ] Keep track of points between sessions? sort of a cache?
- [ ] Remove redundant text in error message.
- [ ] Show progress until next level?
- [ ] Couleur (pride color palette) pour découvrir une nouvelle erreur? 
- [ ] Put .onLoad inside zzz.R
- [ ] Minimal doc
- [ ] Contributor Guidelines
- [ ] mvp for cran sub

## Getting Started

```r
devtools::install("Rwards")
library(Rwards)
trythis
```
