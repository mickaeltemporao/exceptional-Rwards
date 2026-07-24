# Contributing to Rwards

Contributions are welcome! Please feel free to submit issues and pull requests to help make the R learning experience better for everyone.

## Getting Started

1. Fork the repository
2. Create your feature branch (`git checkout -b feature/amazing-feature`)
3. Commit your changes (`git commit -m 'Add amazing feature'`)
4. Push to the branch (`git push origin feature/amazing-feature`)
5. Open a Pull Request

## Development Guidelines

* **Follow R coding conventions**: We recommend following the [tidyverse style guide](https://style.tidyverse.org/). You can use the `styler` and `lintr` packages to format and check your code.
* **Add tests for new features**: This project uses `testthat` for unit testing. Please add corresponding tests in the `tests/` directory when introducing new features or fixing bugs.
* **Update documentation as needed**: Documentation is generated with `roxygen2`. The roxygen comments above each function in `R/` are the single source of truth, and `man/*.Rd` and `NAMESPACE` are generated, not hand-edited. If you change or add a function, update its roxygen comments (`@param`, `@return`, `@examples`, `@export`) and run `devtools::document()` to regenerate the `man/` folder and `NAMESPACE` before committing.
* **Run checks before committing**: Always run `devtools::check()` locally before opening a pull request to ensure there are no errors, warnings, or notes.

## Other Ways to Contribute

We are building a tool to encourage newcomers, so there are many ways you can help beyond just code:

### Translations
We want `Rwards` to be accessible globally. If you're fluent in another language, contributing translations for our encouraging messages, reward notifications, and documentation would be incredible. Open an issue to let us know which language you'd like to help with!

### Themes and Visuals
Since the package relies on `crayon` and `progress` for terminal output, any ideas to make the terminal experience more engaging are highly welcome! You can contribute:
* New color schemes or output themes for when users "level up".
* ASCII art or emojis to accompany error messages.
* Fun, customized progress bars.

### New "Mistake" Categories
R has a lot of unique quirks. If you know of common beginner pitfalls, you can contribute by writing tailored, encouraging messages for those specific errors. 

If you have any questions, don't hesitate to open an issue and ask!
