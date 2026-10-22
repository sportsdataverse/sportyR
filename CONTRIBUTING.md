# Contributing to `sportyR`

First of all, thanks for considering contributing to `sportyR`! The package, just like all open source projects, can't improve without contributions and feedback from its users.

`sportyR` is a labor of love, and no one is being paid to work on it. Please keep this in mind throughout your contributions, as responses and bug fixes may take a bit of time to work through.

## Play by the Rules - Code of Conduct

Please note that this project is released with a [Contributor Code of Conduct](CODE-OF-CONDUCT.md). By participating in this project, you're agreeing to abide by its terms.

`sportyR`'s League Office (see below) reserves the right to limit and/or exclude contributions from those who violate the Code of Conduct as is appropriate.

## Getting In The Game - Making Contributions

There's many ways that you can contribute to `sportyR`. If you want a more complete explanation of how you can contribute (to any open source project, not just this one), check out [this guide](https://opensource.guide/how-to-contribute/).

### Be A Fan - Love the Package

Think that `sportyR` is awesome? Want to share the awesome plots and gifs you made with it? Let others discover it by telling them in person, tagging the project on [X](https://x.com/sportyR_pkg), or writing a blog post about your experience with it!

If you're using `sportyR` in an academic context, please do your best to cite the package as well.

### Scouting Reports - Bugs and Feature Suggestions

Like any good sports organization, `sportyR` does its best to scout for itself and solve problems before they become problems. Given a tough test, problems are sure to arise. They can be overcome in their next match, however, by making use of good scouting (bug) reports. Please create an [Issue](https://github.com/sportsdataverse/sportyR/issues/new/choose) when you find something that needs to be fixed, picking the template that fits best: **Bug report**, **Dimension correction**, **New sport or league**, or **Feature request**. Here's a good general guide to follow for creating one:

1) Please double check the [README](https://www.github.com/sportsdataverse/sportyR) and the [Issues](https://github.com/sportsdataverse/sportyR/issues) pages to be sure that your problem hasn't already been solved. This isn't meant to stop you from reporting your problem, but rather to be sure that it's a new one so it can be properly addressed.

2) Title your issue with a clear description of what you're looking for. Titling your Issue "This is weird..." doesn't give as good of an idea of what you're after as titling your Issue "NCAA Football hashmarks are out of place." The issue templates add a title prefix (and, for most, a label) for you

3) Spend some time creating a detailed explanation, with a reproducible example, of the problem you're having. Without being able to understand the issue, it's even harder to find what's causing it to occur. Do your best to make sure your code is well-formatted and easy for others to read:

    - Make sure you name your variables something informative

    - Use comments to indicate where you believe the problem in your code is occurring

    - Only include the necessary code to recreate the problem in your Issue report so it can be solved correctly

### The League Office Directory - Contributor Status

Want to join `sportyR`'s team and help shape the package? Great! We're always recruiting! Here's how you can join:

- **League Office** &mdash; By contributing regularly, providing substantial upgrades to the package's overall structure and function, and helping to maintain the package across the board, you'll be considered for membership in the `sportyR`'s **League Office**.

    In order to join the League Office, you must be invited by a current member and get approved by a majority of the other members of the League Office

    When you become a League Officer, you should list yourself in the [README](https://github.com/sportsdataverse/sportyR), and also list yourself in the [DESCRIPTION](https://github.com/sportsdataverse/sportyR/blob/main/DESCRIPTION) file with the role of `"aut"`

- **General Managers** &mdash; Writing the code that extends `sportyR`'s sport-plotting reach by developing for new sports will earn you a spot as a **General Manager**. Please list yourself (in alphabetical-by-sport order) in the [README](https://github.com/sportsdataverse/sportyR) with what sport you've added.
    
    Note that by contributing a new sport, you'll be considered the General Manager of that sport. You may be asked to assist in sport-specific issues if the League Office is unable to provide a clear answer

- **Coaching Staffs** &mdash; Patching a bug in an already-existing sport means you're eligible to be on that sport's **coaching staff**. You've got your eye on the game, and it's much appreciated! Coaches can list themselves in the README as well, similar to how General Managers are listed

- **Scout Team** &mdash; By fixing a typo, making a slight modification to something that already exists in `sportyR`, or regularly reporting issues with the package that you encounter (or even helping others solve theirs), you can join the **Scout Team**. Thanks for being on the front lines of the package's operation! Feel free to list yourself in the [README](https://github.com/sportsdataverse/sportyR) under **Scout Team** with a brief description of your role ("Typo Fixer" or "Question Answerer" work great, but be as creative with it as you'd like)

## Becoming a Coach/General Manager - Pull Request Process

To become a Coach or General Manager, submit a [Pull Request](https://github.com/sportsdataverse/sportyR/pulls) (PR) on GitHub using this process:

1) Create a branch off of `main` and make the changes you wish to see implemented. Name the branch after the type of change, e.g. `fix/nfl-hashmarks` or `feat/add-cricket`

2) Run the same checks that CI runs (see **Continuous Integration** below) and fix anything they flag

3) Push your branch to GitHub and open your PR against `main`. Title your PR using [Conventional Commits](https://www.conventionalcommits.org/) (see **PR Titles and Releases** below) so that it's obvious what its overarching goal is, and fill in the PR template's checklist

4) Discuss the PR in as much detail as possible. Provide links to your sources, and clearly explain why the PR is necessary and how it can be used. Any deviations from the conventions of the rest of the package should be clearly explained, but only if the deviation is necessary

5) Continue iterating your request until the League Office determines if the addition is a good fit for `sportyR`

Your request will be checked against the following points:

- **Motivation** &mdash; Clearly and concisely explain what motivated the PR. If it addresses a particular issue, be sure you link to it

- **Only Related Changes** &mdash; Before you submit your PR, make sure your PR only addresses *one* issue. If you want to fix multiple issues, please address them in separate PRs so that it's clear what change is being made in each PR. Please also be sure that your PR doesn't introduce any new bugs or break any existing code

- **Code Style and File Naming** &mdash; Please follow [the tidyverse styleguide](https://style.tidyverse.org/), with the only deviation being explicit calls to `return()` in order to return what's needed from each function. Maintaining consistent styling facilitates easier current maintenance and future development.

    Additionally, please keep consistent with the naming of new files, especially when adding a new sport. Files that contain a new league should be named as `features-{sport}.R`, and if this is a new sport entirely, be sure to add the corresponding `geom-{new sport here}.R` file as well. Dimensions should be maintained in the `data-raw/surface-dimensions.json` file, and `data-raw/internal-datasets.R` should be sourced to incorporate new changes. Please parameterize the surface to the best of your abilities, and ask with any questions!

- **Documentation** &mdash; When introducing new code, please make sure you document it appropriately. Documentation comes in two main forms: `roxygen2` documentation for the functions you write, and comments throughout the code that explain what your code is trying to achieve. Both types of documentation are very important! Be sure to run `devtools::document()` to be sure your documentation files are properly created before submitting, and be sure to check for spelling and grammar.
    
    A quick note on internal/helper function documentation: please make sure that you omit these files from the package build by using `@keywords internal` in your `roxygen2` documentation. This reduces the overall size of the package when being submitted to CRAN, which only allows a maximum package size of 5MB

- **Testing** &mdash; `sportyR` relies upon extensive testing to ensure that the package runs smoothly. It currently has nearly 100% code coverage, and your PR should not lower it.

    To test your coverage, be sure to have the `covr` package installed and run the following commands:

    ```
    cov <- covr::package_coverage()
    covr::zero_coverage(cov)
    ```

    This will show you where your new code lacks sufficient coverage, so please be sure to add tests in the `tests/` repository to make sure you're covered! For help on creating tests, please reach out to the League Office

### Continuous Integration

PRs against `main` run the following GitHub Actions workflows, and any that run should pass before a PR is merged. Each one only runs when the PR changes the files it checks, so, for example, a PR that only touches the README skips the first three:

- **R-CMD-check** &mdash; `R CMD check` on macOS, Windows, and Ubuntu, across R-devel, the current release, and the previous release. Runs when `R/`, `tests/`, `man/`, `inst/`, `DESCRIPTION`, `NAMESPACE`, or `.Rbuildignore` change. Run `devtools::check()` locally to catch problems first

- **lint** &mdash; `lintr::lint_package()` with the settings in `.lintr`. Runs when `R/`, `tests/`, or `.lintr` change. Run it locally and fix any lints your changes introduce

- **test-coverage** &mdash; runs the test suite through `covr` and reports the result to [Codecov](https://codecov.io/gh/sportsdataverse/sportyR). Runs when `R/`, `tests/`, `DESCRIPTION`, or `NAMESPACE` change

- **pkgdown** &mdash; builds the [package website](https://sportyR.sportsdataverse.org). Runs when the R code, documentation, vignettes, README, NEWS, or site configuration change. Run `pkgdown::build_site()` locally if you've changed documentation, vignettes, or `_pkgdown.yml`

### PR Titles and Releases

PRs are squash-merged, so your PR title becomes the commit message on `main`. Releases are cut automatically from those messages by [semantic-release](https://semantic-release.gitbook.io/), which means the PR title decides whether (and how) the version number changes. Please title your PR as `type(scope): summary`, for example `fix(football): place NCAA hashmarks at 60 ft`:

- `fix:` or `perf:` &mdash; a patch release (e.g. 2.4.0 to 2.4.1)

- `feat:` &mdash; a minor release (e.g. 2.4.0 to 2.5.0)

- `feat!:`, `fix!:`, or a `BREAKING CHANGE:` footer &mdash; a major release (e.g. 2.4.0 to 3.0.0). Use this only for changes that break existing user code

- `docs:`, `test:`, `refactor:`, `ci:`, `build:`, `chore:` &mdash; no release

The scope is optional, but naming the sport (`basketball`, `lacrosse`, ...) or area (`pkgdown`, `ci`, ...) you changed is helpful.

Because the release workflow handles versioning, please **don't** change the `Version` field in `DESCRIPTION` or add a new version heading to `NEWS.md`. When a release is cut, the workflow bumps `DESCRIPTION`, adds the release notes to `NEWS.md`, tags the release, and comments on the PRs and issues it includes. If you'd like to describe your change in more detail than the PR title, add your notes under a `# sportyR (development version)` heading at the top of `NEWS.md` (create the heading if it isn't there). Those notes will be folded into the next release's section.

Although this seems like a lot of work and a big hassle, the League Office doesn't expect perfection and is on hand to help out as needed.
