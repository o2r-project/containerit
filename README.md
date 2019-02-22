
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
[![](http://www.r-pkg.org/badges/version/containerit)](http://www.r-pkg.org/pkg/containerit)

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
#> INFO [2019-07-19 14:27:51] Going online? TRUE  ... to retrieve system dependencies (sysreq-api)
#> INFO [2019-07-19 14:27:51] Trying to determine system requirements for the package(s) 'Rcpp,digest,futile.options,semver,formatR,magrittr,evaluate,stringi,curl,futile.logger,rmarkdown,lambda.r,stringr,xfun,yaml,stevedore,htmltools,knitr' from sysreqs online DB
#> INFO [2019-07-19 14:27:53] Adding CRAN packages: curl, digest, evaluate, formatR, futile.logger, futile.options, htmltools, knitr, lambda.r, magrittr, Rcpp, rmarkdown, semver, stevedore, stringi, stringr, xfun, yaml
#> INFO [2019-07-19 14:27:53] Created Dockerfile-Object based on sessionInfo
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
#> RUN ["install2.r", "curl", "digest", "evaluate", "formatR", "futile.logger", "futile.options", "htmltools", "knitr", "lambda.r", "magrittr", "Rcpp", "rmarkdown", "semver", "stevedore", "stringi", "stringr", "xfun", "yaml"]
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
#> O> "","x"
#> O> "askpass","1.1"
#> O> "assertthat","0.2.0"
#> O> "backports","1.1.3"
#> O> "base64enc","0.1-3"
#> O> "BH","1.69.0-1"
#> O> "BiocManager","1.30.4"
#> O> "bit","1.1-14"
#> O> "bit64","0.9-7"
#> O> "bitops","1.0-6"
#> O> "blob","1.1.1"
#> O> "bookdown","0.9"
#> O> "brew","1.0-6"
#> O> "broom","0.5.1"
#> O> "callr","3.1.1"
#> O> "caTools","1.17.1.2"
#> O> "cellranger","1.1.0"
#> O> "cli","1.0.1"
#> O> "clipr","0.5.0"
#> O> "clisymbols","1.2.0"
#> O> "colorspace","1.4-0"
#> O> "commonmark","1.7"
#> O> "covr","3.2.1"
#> O> "crayon","1.3.4"
#> O> "curl","3.3"
#> O> "data.table","1.12.0"
#> O> "DBI","1.0.0"
#> O> "dbplyr","1.3.0"
#> O> "desc","1.2.0"
#> O> "devtools","2.0.1"
#> O> "digest","0.6.18"
#> O> "docopt","0.6.1"
#> O> "dplyr","0.8.0.1"
#> O> "dtplyr","0.0.3"
#> O> "ellipsis","0.1.0"
#> O> "evaluate","0.13"
#> O> "fansi","0.4.0"
#> O> "feather","0.3.2"
#> O> "foghorn","1.1.0"
#> O> "forcats","0.4.0"
#> O> "formatR","1.6"
#> O> "fs","1.2.6"
#> O> "generics","0.0.2"
#> O> "ggplot2","3.1.0"
#> O> "gh","1.0.1"
#> O> "git2r","0.24.0"
#> O> "glue","1.3.1"
#> O> "gmailr","0.7.1"
#> O> "gtable","0.2.0"
#> O> "haven","2.1.0"
#> O> "highlight","0.4.7.2"
#> O> "highr","0.7"
#> O> "hms","0.4.2"
#> O> "htmltools","0.3.6"
#> O> "htmlwidgets","1.3"
#> O> "httpuv","1.4.5.1"
#> O> "httr","1.4.0"
#> O> "hunspell","3.0"
#> O> "igraph","1.2.4"
#> O> "ini","0.3.1"
#> O> "jsonlite","1.6"
#> O> "knitr","1.22"
#> O> "labeling","0.3"
#> O> "Lahman","6.0-0"
#> O> "later","0.8.0"
#> O> "lazyeval","0.2.1"
#> O> "lintr","1.0.3"
#> O> "littler","0.3.6"
#> O> "lubridate","1.7.4"
#> O> "magrittr","1.5"
#> O> "markdown","0.9"
#> O> "memoise","1.1.0"
#> O> "microbenchmark","1.4-6"
#> O> "mime","0.6"
#> O> "miniUI","0.1.1.1"
#> O> "mockery","0.4.1.1"
#> O> "modelr","0.1.4"
#> O> "munsell","0.5.0"
#> O> "nycflights13","1.0.0"
#> O> "openssl","1.2.2"
#> O> "packrat","0.5.0"
#> O> "parsedate","1.1.3"
#> O> "pillar","1.3.1"
#> O> "pingr","1.1.2"
#> O> "pkgbuild","1.0.2"
#> O> "pkgconfig","2.0.2"
#> O> "pkgdown","1.3.0"
#> O> "pkgload","1.0.2"
#> O> "PKI","0.1-5.1"
#> O> "plogr","0.2.0"
#> O> "plyr","1.8.4"
#> O> "praise","1.0.0"
#> O> "prettyunits","1.0.2"
#> O> "processx","3.3.0"
#> O> "progress","1.2.0"
#> O> "promises","1.0.1"
#> O> "ps","1.3.0"
#> O> "purrr","0.3.1"
#> O> "R6","2.4.0"
#> O> "rappdirs","0.3.1"
#> O> "rcmdcheck","1.3.2"
#> O> "RColorBrewer","1.1-2"
#> O> "Rcpp","1.0.0"
#> O> "RCurl","1.95-4.12"
#> O> "readr","1.3.1"
#> O> "readxl","1.3.1"
#> O> "rematch","1.0.1"
#> O> "rematch2","2.0.1"
#> O> "remotes","2.0.2"
#> O> "reprex","0.2.1"
#> O> "reshape2","1.4.3"
#> O> "rex","1.1.2"
#> O> "rhub","1.0.2"
#> O> "rJava","0.9-10"
#> O> "rlang","0.3.1"
#> O> "rmarkdown","1.11"
#> O> "rmdshower","2.1.1"
#> O> "RMySQL","0.10.17"
#> O> "roxygen2","6.1.1"
#> O> "RPostgreSQL","0.6-2"
#> O> "rprojroot","1.3-2"
#> O> "rsconnect","0.8.13"
#> O> "RSQLite","2.1.1"
#> O> "rstudioapi","0.9.0"
#> O> "rticles","0.6"
#> O> "rversions","1.0.3"
#> O> "rvest","0.3.2"
#> O> "scales","1.0.0"
#> O> "selectr","0.4-1"
#> O> "servr","0.13"
#> O> "sessioninfo","1.1.1"
#> O> "shiny","1.2.0"
#> O> "sourcetools","0.1.7"
#> O> "spelling","2.1"
#> O> "stringdist","0.9.5.1"
#> O> "stringi","1.4.3"
#> O> "stringr","1.4.0"
#> O> "sys","3.1"
#> O> "testit","0.9"
#> O> "testthat","2.0.1"
#> O> "tibble","2.0.1"
#> O> "tidyr","0.8.3"
#> O> "tidyselect","0.2.5"
#> O> "tidyverse","1.2.1"
#> O> "tinytex","0.11"
#> O> "tufte","0.4"
#> O> "usethis","1.4.0"
#> O> "utf8","1.1.4"
#> O> "viridisLite","0.3.0"
#> O> "webshot","0.5.1"
#> O> "whisker","0.3-2"
#> O> "whoami","1.2.0"
#> O> "withr","2.1.2"
#> O> "xfun","0.5"
#> O> "XML","3.98-1.19"
#> O> "xml2","1.2.0"
#> O> "xopen","1.0.0"
#> O> "xtable","1.8-3"
#> O> "yaml","2.2.0"
#> O> "base","3.5.2"
#> O> "boot","1.3-20"
#> O> "class","7.3-14"
#> O> "cluster","2.0.7-1"
#> O> "codetools","0.2-15"
#> O> "compiler","3.5.2"
#> O> "datasets","3.5.2"
#> O> "foreign","0.8-71"
#> O> "graphics","3.5.2"
#> O> "grDevices","3.5.2"
#> O> "grid","3.5.2"
#> O> "KernSmooth","2.23-15"
#> O> "lattice","0.20-38"
#> O> "MASS","7.3-51.1"
#> O> "Matrix","1.2-15"
#> O> "methods","3.5.2"
#> O> "mgcv","1.8-26"
#> O> "nlme","3.1-137"
#> O> "nnet","7.3-12"
#> O> "parallel","3.5.2"
#> O> "rpart","4.1-13"
#> O> "spatial","7.3-11"
#> O> "splines","3.5.2"
#> O> "stats","3.5.2"
#> O> "stats4","3.5.2"
#> O> "survival","2.43-3"
#> O> "tcltk","3.5.2"
#> O> "tools","3.5.2"
#> O> "utils","3.5.2"
print(rmd_dockerfile)
#> FROM rocker/verse:3.5.2
#> LABEL maintainer="o2r"
#> # Packages skipped because they are in the base image: Rcpp, knitr, magrittr, stringr, xfun, sessioninfo, cli, withr, htmltools, yaml, digest, assertthat, crayon, formatR, curl, evaluate, rmarkdown, stringi
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
library(rhub)
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

A [codemeta](https://codemeta.github.io/) file, `codemeta.json` with
metadata about the package and its dependencies is generated
automatically when this document is compiled.

``` r
codemetar::write_codemeta("containerit")
```

## License

containerit is licensed under GNU General Public License, version 3, see
file LICENSE.

Copyright (C) 2019 - o2r project.
