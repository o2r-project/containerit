# Contributing

We welcome [contributions of all kinds](https://opensource.guide/how-to-contribute/), be it code, tests, or documentation.
When contributing to this repository, please first discuss the change you wish to make via issue or in the chat room before making a change: [![Join the chat at https://gitter.im/o2r-project/containerit](https://badges.gitter.im/o2r-project/containerit.svg)](https://gitter.im/o2r-project/containerit)

Check out the [existing issues](https://github.com/o2r-project/containerit/issues) and the [project board](https://github.com/o2r-project/containerit/projects/1), where the development ist planned, for related or similar ideas and features.

**Please note we have a [code of conduct](CONDUCT.md), please follow it in all your interactions with the project.**

## How to contribute

The following steps assume you are familiar with R packages.
If not, take a look at [R extension packages](http://r-pkgs.had.co.nz/) by Hadley Wickham and the [R extension manual](http://cran.r-project.org/doc/manuals/R-exts.html).

[Fork](https://help.github.com/en/articles/fork-a-repo), then clone the repo:

```
git clone https://github.com/o2r-project/containerit.git
```

Install the package from source and make sure the tests pass:

```r
devtools::install()
devtools::test()
```

- Create a new feature branch from the current `master` branch.
- Make your change.
- Add documentation for your change (e.g. function documentation with [`roxygen2`](https://roxygen2.r-lib.org/), a chapter in a vignette, a new vignette)
- Add tests for your change.
- Make the tests pass.
- Make `R CMD check` pass.
- Rebuild the documentation with `pkgdown::build_site()`
- Add a note to `NEWS.md` about your change
- Add yourself to the contributor list and increment the [development version](http://r-pkgs.had.co.nz/description.html#version) in `DESCRIPTION`

Push your feature branch to your fork and [submit a pull request](https://help.github.com/en/articles/about-pull-requests) to the upstream's `master` branch.
Please be patient until maintainers review your pull request.
We may suggest some changes or improvements or alternatives.
Feel free to ping `@nuest` if you don't hear anything after a few weeks.

## Acknowledgements

This guide builds upon the following document:

- https://github.com/thoughtbot/factory_bot_rails/blob/master/CONTRIBUTING.md
- https://github.com/52North/sos4R/blob/master/DEV-README.md
