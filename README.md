
<!-- README.md is generated from README.Rmd. Please edit that file -->

# containerit

`containerit` packages R script/session/workspace and all dependencies
as a [Docker](http://docker.com/) container by automagically generating
a suitable `Dockerfile`.

[![Project Status: WIP - Initial development is in progress, but there
has not yet been a stable, usable release suitable for the
public.](http://www.repostatus.org/badges/latest/wip.svg)](http://www.repostatus.org/#wip)
[![Build
Status](https://travis-ci.org/o2r-project/containerit.svg?branch=master)](https://travis-ci.org/o2r-project/containerit)
[![](http://www.r-pkg.org/badges/version/containerit)](http://www.r-pkg.org/pkg/containerit)

![containerit logo](inst/logo.png)

## Quick start

### Install

Development version from GitHub.

    devtools::install_github("o2r-project/containerit")

### Use

See the vignettes at `vignettes/containerit.Rmd` and
`vignettes/container.Rmd`.

## Contribute

All help is welcome: asking questions, providing documentation, testing,
or even programming.

Please note that this project is released with a [Contributor Code of
Conduct](CONDUCT.md). By participating in this project you agree to
abide by its terms.

## Development

[r-hub builder](https://builder.r-hub.io/) is great for running checks,
e.g. before submitting to CRAN and on other operating systems.

``` r
library(rhub)
rhub::check_for_cran()
rhub::check_on_windows()
```

You can build the [`pkgdown`]() site with

``` r
pkgdown::build_site()
```

## License

containerit is licensed under GNU General Public License, version 3, see
file LICENSE.

Copyright (C) 2018 - o2r project.
