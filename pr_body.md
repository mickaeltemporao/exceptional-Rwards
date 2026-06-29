This PR implements the Diablo 2 inspired XP and leveling system for Rwards.

### Features
- Introduces an XP scale where standard errors grant 50 XP and new errors grant 150 XP.
- Adds level titles and allows users to switch between `basic` and `cyberpunk` themes via `options(Rwards.theme = "cyberpunk")`.
- Integrates persistent caching utilizing `tools::R_user_dir` so progress is saved across R sessions.
- Updates the progress bar format to track XP towards the next level limit.
- Replaces generic error thresholds with random encouraging messages and special level-up announcements.
- Adds a `testthat` suite to verify XP tracking, level thresholding, and themes.

Resolves #4, Resolves #5, Resolves #6
