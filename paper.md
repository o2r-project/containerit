---
title: 'containerit: Generating Dockerfiles for reproducible research with R'
tags:
  - containerisation
  - Docker
  - sandbox
  - reproducibility
  - reproducible research
authors:
  - name: Daniel Nüst
    orcid: 0000-0002-0024-5046
    affiliation: 1
  - name: Matthias Hinz
    orcid: 0000-0001-6837-9406
    affiliation: 2
affiliations:
 - name: Institute for Geoinformatics, University of Münster, Germany
   index: 1
 - name: Professorship for Geoinformatics and Geodesy, Faculty of Agricultural and Environmental Sciences, University of Rostock, Germany
   index: 2
date: 22 July 2019
bibliography: paper.bib
---

# Statement of Need

[Linux containers](https://en.wikipedia.org/wiki/Operating-system-level_virtualization) have become a promising tool to increase transparency, portability, and reproducibility of research in several domains and use cases: data science [@boettiger_introduction_2015], software engineering research [@cito_using_2016], multi-step bioinformatics pipelines [@kim_bio-docklets_2017], standardised environments for exchangeable software [@belmann_bioboxes_2015], computational archaeology [@marwick_computational_2017], packaging algorithms [@hosny_algorun_2016], or geographic object-based image analysis [@knoth_reproducibility_2017].
Running an analysis in a container increases reliability of a workflow, as it can execute packaged code independently of the author's computer and its available configurations and dependencies.
However, capturing a computational environment in containers can be complex, making container use difficult for domain scientists with limited programming experience.
`containerit` opens up the advantages of containerisation to a much larger user base by assisting researchers in packaging workflows based on R [@r_2018] in containers with user-friendly R commands.

Recently containerisation took off as a technology for packaging applications and their dependencies for fast, scalable, and secure [sandboxed](https://en.wikipedia.org/wiki/Sandbox_%28computer_security%29) deployments in cloud-based infrastructures [cf. @osnat_brief_2018].
The most widely used containerisation software is [Docker](https://en.wikipedia.org/wiki/Docker_%28software%29) with the following core building blocks (cf. [Docker: Get Started](https://docs.docker.com/get-started/)):
The _image_ is built from the instructions in a recipe called `Dockerfile`.
The image is executed as a _container_ using a _container runtime_.
An image can be moved between systems as a file (image tarball) or based on an _image registry_.
A `Dockerfile` may use the image created by another `Dockerfile` as the starting point, a so-called _base image_.
While containers can be manually altered, the common practice is to conduct all configurations with the scripts and instructions originating in the `Dockerfile`.

An important advantage of containers over [virtual machines](https://en.wikipedia.org/wiki/Virtual_machine) is that their duality between recipe and image provides and additional layer of transparency and safeguarding.
The `Dockerfile` and image can be published alongside a scientific paper to support peer review and, to some extent, preserve the original results [@nust_opening_2017].
Even if an image cannot be executed or a `Dockerfile` can no longer be built, the instructions in the `Dockerfile` are human-readable, and files in the image can be extracted to recreate an environment that closely resembles the original.
Further useful features are (a) portability, thanks to a single runtime dependency, which allows readers to explore an author's virtual laboratory, including complex dependencies or custom-made code, either on their machines or in cloud-based infrastructures [e.g., by using Binder, see @jupyter_binder_2018], and (b) transparency, because an image's filesystem can be easily inspected.
This way, containers can enable verification of reproducibility and auditing without requiring reviewers to manually download, install, and re-run analyses [@beaulieu-jones_reproducibility_2017].

Container preservation is an active field of research [@rechert_preserving_2017; @emsley_framework_2018].
It is reasonable to assume that key stakeholders interested in workflow preservation, such as universities or scientific publishers, should be able to operate container runtimes on a time scale comparable to data storage requirements by funding agencies, e.g. 10 years in case of the [German DFG](http://www.dfg.de/en/research_funding/proposal_review_decision/applicants/research_data/index.html) or [British EPSRC](https://epsrc.ukri.org/about/standards/researchdata/expectations/).
To enable and leverage the stakeholders' infrastructure, container creation must become easier and more widespread.

# Summary

The package `containerit` automates the generation of `Dockerfile`s for workflows in R, based on images by the [Rocker project](https://www.rocker-project.org/) [@RJ-2017-065].
The core feature of `containerit` is that it transforms the local session information into a set of instructions which can be serialised as a `Dockerfile`, as shown in the code snippet below:

```R
> suppressPackageStartupMessages(library("containerit"))
> my_dockerfile <- containerit::dockerfile(from = utils::sessionInfo())
> print(my_dockerfile)
FROM rocker/r-ver:3.5.2
LABEL maintainer="daniel"
RUN export DEBIAN_FRONTEND=noninteractive; apt-get -y update \
  && apt-get install -y git-core \
	libcurl4-openssl-dev \
	libssl-dev \
	pandoc \
	pandoc-citeproc
RUN ["install2.r", "curl", "digest", "evaluate", "formatR", \
  "futile.logger", "futile.options", "htmltools", "jsonlite", \
  "knitr", "lambda.r", "magrittr", "Rcpp", "rjson", \
  "rmarkdown", "rsconnect", "semver", "stevedore", "stringi", \
  "stringr", "xfun", "yaml"]
WORKDIR /payload/
CMD ["R"]
```

The created `Dockerfile` has installation instructions for the loaded packages and their system dependencies.
It uses the `r-ver` stack of Rocker images, matching the R version to the environment encountered locally by `containerit`.
These images use [MRAN](https://mran.microsoft.com/) snapshots to control installed R package versions in a reproducible way.
The system dependencies required by these packages are identified using the `sysreqs` package [@csardi_sysreqs_2019] and the corresponding [database and API](http://sysreqs.r-hub.io/).

`dockerfile(..)` is the package's main user function and accepts session information objects, session information saved in a file, a set of R commands, an R script file, a [`DESCRIPTION`](https://cran.r-project.org/doc/manuals/r-release/R-exts.html#The-DESCRIPTION-file) file, or an R Markdown document [@allaire_rmarkdown_2018].
[Static program analysis](https://en.wikipedia.org/wiki/Static_program_analysis) using the package `automagic` [@brokamp_automagic_2017] is used to increase the chances that the capturing environment has all required packages available, such as when creating Dockerfiles for R Markdown documents as a service [@nust_reproducibility_2018].
To capture the workflow environment, `containerit` executes the whole workflow in a new R session using the package `callr` [@csardi_callr_2018], because static program analysis can be broken by using helper functions, such as `xfun::pkg_attach()` [@xie_xfun_2018], by unintended side effects, or by seemingly clever or user-friendly yet customised ways of loeading packages (cf. first lines in R script file `tgis_a_1579333_sm7524.r` in [https://doi.org/10.6084/m9.figshare.7757069.v1](https://doi.org/10.6084/m9.figshare.7757069.v1)).
Further parameters for the function comprise, for example, image metadata, base image, versioned installations, and filtering of R packages already installed in the base image.

The package `containerit`'s main contribution is that it allows for automated capturing of runtime environments as `Dockerfile`s based on literate programming workflows [@gentleman_statistical_2007] to support reproducible research.
Together with `stevedore` [@fitzjohn_stevedore_2019], `containerit` enables a completely R-based creation and manipulation of Docker containers.
Using `containerit` only minimally affects researchers' workflows because it can be applied after completing a workflow, while at the same time the captured snapshots can enhance the scholarly publication process (in particular review, interaction, and preservation) and may form a basis for more reusable and transparent publications.
In the future, `containerit` may support alternative container software such as Singularity [@kurtzer_singularity_2017], enable parametrisation of container executions and pipelines as demonstrated by Kliko [@molenaar_klikoscientific_2018], or support proper accreditation of software [@codemeta, @katz_software_2018].

**Related Work**

[The Experiment Factory](https://expfactory.github.io/) similarly focuses on ease of use for creating `Dockerfile`s for behavioural experiments, yet it uses a CLI-based interaction and generates extra shell scripts to be included in the images.
[ReproZip](https://www.reprozip.org/) [@ChirigatiRSF16] packages files identified by [tracing](https://en.wikipedia.org/wiki/Tracing_(software)) in a self-contained bundle, which can be unpacked to a Docker container/`Dockerfile`.
In the R domain, the package `dockerfiler` [@fay_dockerfiler_2018] provides an object-oriented API for manual Dockerfile creation, and `liftr` [@xiao_liftr_2018] creates a `Dockerfile` based on fields added to the metadata header of an R Markdown document.
`automagic` [@brokamp_automagic_2017], [Whales](https://github.com/Gueils/whales), [`dockter`](https://github.com/stencila/dockter/), and [`repo2docker`](https://github.com/jupyter/repo2docker) use static program analysis to create environment descriptions from common project configuration files for multiple programming languages.
Namely, `automagic` analyses R code and can store dependencies in a bespoke [YAML](https://en.wikipedia.org/wiki/YAML) format.
Whales and `dockter` provide different formats, including `Dockerfile`.
Finally, `repo2docker` primarily creates containers for interactive notebooks to run as a Binder [@jupyter_binder_2018] but does not actively expose a `Dockerfile`.
None of them apply the strict code execution approach as `containerit` does.

# Acknowledgements

This work is supported by the project [Opening Reproducible Research](https://o2r.info) ([Offene Reproduzierbare Forschung](https://www.uni-muenster.de/forschungaz/project/9520)) funded by the German Research Foundation ([DFG](http://dfg.de/)) under project numbers `PE 1632/10-1` and `1632/17-1`.

# References
