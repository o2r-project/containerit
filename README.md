
<!-- README.md is generated from README.Rmd. Please edit that file -->

# containerit

`containerit` packages R script/session/workspace and all dependencies
as a [Docker](http://docker.com/) container by automagically generating
a suitable `Dockerfile`. The packages’s website at
<https://o2r.info/containerit/>. A good summary of what the package does
can be found in this [o2r project blog
post](http://o2r.info/2017/05/30/containerit-package/).

[![Project Status: WIP - Initial development is in progress, but there
has not yet been a stable, usable release suitable for the
public.](http://www.repostatus.org/badges/latest/wip.svg)](http://www.repostatus.org/#wip)
[![Build
Status](https://travis-ci.org/o2r-project/containerit.svg?branch=master)](https://travis-ci.org/o2r-project/containerit)
[![AppVeyor build
status](https://ci.appveyor.com/api/projects/status/github/containerit-rrvpq/containerit?branch=master&svg=true)](https://ci.appveyor.com/project/containerit-rrvpq/containeRit)
[![](https://www.r-pkg.org/badges/version/containerit)](#)
<!-- https://www.r-pkg.org/pkg/containerit -->

![containerit logo](inst/logo.png)

## Quick start

### Install

Installation is only possible from GitHub:

    remotes::install_github("o2r-project/containerit")

### Use

`containerit` can create `Dockerfile` objects in R and render them as
`Dockerfile` instructions based on session information objects or
runnable R files (`.R`, `.Rmd`).

``` r
suppressPackageStartupMessages(library("containerit"))
my_dockerfile <- containerit::dockerfile(from = utils::sessionInfo())
#> INFO [2019-07-21 14:07:25] Going online? TRUE  ... to retrieve system dependencies (sysreq-api)
#> INFO [2019-07-21 14:07:25] Trying to determine system requirements for the package(s) 'assertthat,backports,crayon,curl,desc,digest,evaluate,formatR,futile.logger,futile.options,htmltools,knitr,lambda.r,magrittr,R6,Rcpp,rmarkdown,rprojroot,semver,stevedore,stringi,stringr,xfun,yaml' from sysreqs online DB
#> INFO [2019-07-21 14:07:27] Adding CRAN packages: assertthat, backports, crayon, curl, desc, digest, evaluate, formatR, futile.logger, futile.options, htmltools, knitr, lambda.r, magrittr, R6, Rcpp, rmarkdown, rprojroot, semver, stevedore, stringi, stringr, xfun, yaml
#> INFO [2019-07-21 14:07:27] Created Dockerfile-Object based on sessionInfo
```

``` r
print(my_dockerfile)
#> FROM rocker/r-ver:3.6.1
#> LABEL maintainer="daniel"
#> RUN export DEBIAN_FRONTEND=noninteractive; apt-get -y update \
#>   && apt-get install -y git-core \
#>  libcurl4-openssl-dev \
#>  libssl-dev \
#>  pandoc \
#>  pandoc-citeproc
#> RUN ["install2.r", "assertthat", "backports", "crayon", "curl", "desc", "digest", "evaluate", "formatR", "futile.logger", "futile.options", "htmltools", "knitr", "lambda.r", "magrittr", "R6", "Rcpp", "rmarkdown", "rprojroot", "semver", "stevedore", "stringi", "stringr", "xfun", "yaml"]
#> WORKDIR /payload/
#> CMD ["R"]
```

You can disable logging:

``` r
futile.logger::flog.threshold(futile.logger::ERROR)
```

Now create a Dockerfile for a specific R version and R Markdown file and
do not add any packages already available in the base
image:

``` r
rmd_dockerfile <- containerit::dockerfile(from = "inst/demo.Rmd", image = "rocker/verse:3.5.2", maintainer = "o2r", filter_baseimage_pkgs = TRUE)
#> Warning in dockerfileFromFile(file = from, dockerfile = the_dockerfile, :
#> All paths in copy are NULL or NA, not adding any COPY instructions: NA
print(rmd_dockerfile)
#> FROM rocker/verse:3.5.2
#> LABEL maintainer="o2r"
#> # CRAN packages skipped because they are in the base image: assertthat, backports, cli, crayon, curl, desc, digest, evaluate, formatR, htmltools, knitr, magrittr, R6, Rcpp, rmarkdown, rprojroot, rstudioapi, sessioninfo, stringi, stringr, withr, xfun, yaml
#> RUN ["install2.r", "fortunes", "futile.logger", "futile.options", "lambda.r", "semver", "stevedore"]
#> WORKDIR /payload/
#> CMD ["R"]
```

For extended instructions, see the vignettes at in the directory
`vignettes/`, which are readable online at
<https://o2r.info/containerit/articles/>.

### Containers

Images are available starting from different base images. All images are
also available with version tags.

#### verse

Base image:
`rocker/verse:3.5.3`

[![](https://images.microbadger.com/badges/version/o2rproject/containerit.svg)](https://microbadger.com/images/o2rproject/containerit "Get your own version badge on microbadger.com")
[![](https://images.microbadger.com/badges/image/o2rproject/containerit.svg)](https://microbadger.com/images/o2rproject/containerit "Get your own image badge on microbadger.com")
[![](https://images.microbadger.com/badges/commit/o2rproject/containerit.svg)](https://microbadger.com/images/o2rproject/containerit "Get your own commit badge on microbadger.com")

#### geospatial

Base image:
`rocker/geospatial:3.5.3`

[![](https://images.microbadger.com/badges/version/o2rproject/containerit:geospatial.svg)](https://microbadger.com/images/o2rproject/containerit:geospatial "Get your own version badge on microbadger.com")
[![](https://images.microbadger.com/badges/image/o2rproject/containerit:geospatial.svg)](https://microbadger.com/images/o2rproject/containerit:geospatial "Get your own image badge on microbadger.com")
[![](https://images.microbadger.com/badges/commit/o2rproject/containerit:geospatial.svg)](https://microbadger.com/images/o2rproject/containerit:geospatial "Get your own commit badge on microbadger.com")

## RStudio Add-in

[RStudio Addins](https://rstudio.github.io/rstudioaddins/) allow to
create interactive user interfaces for the RStudio development
environment. Courtesy of a great contribution by a [group of
enthusiasts](https://github.com/o2r-project/containerit/issues/27#issuecomment-440869329)
at the [ROpenSci OZ
Unconference 2018](https://ozunconf18.ropensci.org/), there are several
forms to quickly create `Dockefile`s from different use cases, e.g. the
current session, a vector of expressions, or a script file.

![screenshots containerit RStudio Addin
forms](https://user-images.githubusercontent.com/1325054/61534429-e1345980-aa2f-11e9-8f5d-e6f67e5d7dde.png)

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
library("rhub")
rhub::check_for_cran()
rhub::check_on_windows()
```

You can build the [`pkgdown`](http://pkgdown.r-lib.org/) site with

``` r
pkgdown::build_site()
```

You can build the Docker images locally with the current development
version using the following commands.

``` bash
docker build --tag containerit:dev --file inst/docker/Dockerfile.local .
docker build --tag containerit:geospatial-dev --file inst/docker/geospatial/Dockerfile.local .
```

You can use [`pre-commit`
hooks](https://github.com/lorenzwalthert/pre-commit-hooks) to avoid some
mistakes.

## License

containerit is licensed under GNU General Public License, version 3, see
file LICENSE.

Copyright (C) 2019 - o2r project.
