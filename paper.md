---
title: 'containerit: Generating Dockerfiles for reproducible research with R'
tags:
  - containerisation
  - Docker
  - sandbox
  - reproducibility
  - reproducible research
author:
  - name: Daniel Nüst
    orcid: 0000-0002-0024-5046
  - name: Matthias Hinz
    orcid: 0000-0001-6837-9406
date: 31 January 2019
bibliography: paper.bib
---

# Introduction

[Linux containers](https://en.wikipedia.org/wiki/Operating-system-level_virtualization) have been demonstrated as a promising tool to increase transparency, portability, and reproducibility of research in a variety of scholarly domains and use cases: data science [@boettiger_introduction_2015], software engineering reserach [@cito_using_2016], multi-step bioinformatics pipelines [@kim_bio-docklets_2017], standardised environments for exchangeable software [@belmann_bioboxes_2015], computational archeology [@marwick_computational_2017], packaging algorithms [@hosny_algorun_2016], or geographic object-based image analysis [@knoth_reproducibility_2017].
However, especially domain scientists, who often have limited programming experience, struggle with the complexity of managing and capturing their computational environment in containers.
The software `containerit` assists researchers in packaging workflows and documents for R [@r_2018] to increase reproducibility.
While computer scientists and software developers are often capable to create and manage containers by hand, the automatic generation of containers with user-friendly R commands opens up containerisation's advantages to a much larger user base.

# Containers for Science

Originally created for isolation of software and assigning operating system resources, containerisation took off as a technology for packaging server applications and their dependencies for fast, scalable, and secure deployments in cloud-based infrastructures [cf. @osnat_brief_2018].
Containers enable most advantages virtulisation and cloud computing offer for computational science, such as capturing digital laboratories of in-silico experiments, managing dependencies without constraints on research methods, packaging and re-depoy of complex architectures, and new approaches to handling costs of hosting scientific results [@howe_virtual_2012].

The core building block of a container-based infrastructure is the _image_, which can be executed in one or many instances, the _container_, using a _container runtime_.
Images are moved between systems as files (image tarballs) or based on an _image registry_.
The most widely used containerisation software is [Docker](https://en.wikipedia.org/wiki/Docker_(software)).
The recipees with instructions what to include in an image is the `Dockerfile`.
A `Dockerfile` may use the image created by another `Dockerfile` as the starting point, the _base image_, of it's own definition.

An important advantage of a containers compared to a [virtual machine](https://en.wikipedia.org/wiki/Virtual_machine) is the duality between the recipe for creating a container and the image built with it.
While manual alteration of containers is possible, it is a best practice to conduct all configuration with scripts and command-line instructions originating in the `Dockerfile`.

Portability, transparency, and the reduction to a single runtime dependency, i.e. the container runtime, are more important than security (e.g. [sandbox](https://en.wikipedia.org/wiki/Sandbox_(computer_security))) for scholarly applications, because authorship is usually transparent and usage is limited to specific procedures while reviewing or inspecting a piece of research.
The same virtual laboratory a researcher uses locally, including potentially complex system dependencies and bespoke libraries and code, can be packaged, moved, and easily re-opened by other researchers either on their machines or in cloud-based infrastructures [e.g by using Binder, see @jupyter2018binder].
This way containers have been shown to enable verification of reproducibility and auditing without requiring reviewers to manually download, install, and re-run analyses [@beaulieu-jones_reproducibility_2017].
Running the analysis within a container greatly increases trust in the stability of a workflow as it can emulate a second, independently conducted execution of the code.
The `Dockerfile` and image may be published alongside a scientific paper to support peer review and to some extend archival [@nust_opening_2017].
Container preservation is an active field of research [@rechert_preserving_2017; @emsley_framework_2018].
It is reasonable to assume that the operation of a container runtime at a time scale comparable to data storage requirements by funding agencies, e.g. 10 years in case of the [German DFG](http://www.dfg.de/en/research_funding/proposal_review_decision/applicants/research_data/index.html) or [British EPSRC](https://epsrc.ukri.org/about/standards/researchdata/expectations/), is feasible for stakeholders such as universities or scientific publishers, considering the benefits of doing so and the potential for collaboration between stakeholders.
Even if an image cannot be executed, or a `Dockerfile` cannot be build anymore, the instructions in the `Dockerfile` are human-readable and files in the image can be extracted.
These steps enable the recreation of an environment from scratch resembling the original virtual laboratory as closely as possible.

# The package `containerit`

`containerit` automates the generation of `Dockerfile`s for workflows in R based on images provided by the Rocker project [@RJ-2017-065], though other base images may be configured.
The core function is transforming the local session information into a set of instructions, which can be serialised as a `Dockerfile`, as shown in the code snippet below:

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
RUN ["install2.r", "curl", "digest", "evaluate", "formatR", "futile.logger", "futile.options", "htmltools", "jsonlite", "knitr", "lambda.r", "magrittr", "Rcpp", "rjson", "rmarkdown", "rsconnect", "semver", "stevedore", "stringi", "stringr", "xfun", "yaml"]
WORKDIR /payload/
CMD ["R"]
```

The created `Dockerfile` uses the `r-ver` stack of Rocker images, matching the R version to the environment encountered locally by `containerit`.
It has installation instructions for the loaded packages, which in the example above contain packages required to render a preview of this document.
These images use [MRAN](https://mran.microsoft.com/) snapshots to control installed R package versions in a reproducible way.
The system dependencies required by these packages are identified using the `sysreqs` package [@csardi_sysreqs] and the [corresponding database and API](http://sysreqs.r-hub.io/).
The focus on the R environment also allows `containerit` to handle complex configurations effectively, e.g. if an R package requires a specific set of prerequisites.

`dockerfile(..)` is the package's core function.
It accepts a set of R commands or a path to an R script file or an R Markdown document [@allaire_rmarkdown_2018].
To capture a session information object suitable for the workflows embedded in these inputs, `containerit` executes the R code in a new R session using the package `callr` [@callr].
[Static program analysis](https://en.wikipedia.org/wiki/Static_program_analysis) using the package `automagic` [@automagic] is used to make sure the capturing environment has all required packages available, e.g. when creating Dockerfiles for R Markdown documents provided by a third party as a service [@nust_reproducibility_2018].
To retrieve the actual packages, it always executes the full workflow because static program analysis can be broken by using helper functions such as `xfun::pkg_attach()` [@xie_xfun_2018], unintended side-effects, or seemingly clever but bespoke ways to load packages.
Advanced features include generation of installation statements for specific R package versions and filtering R packages already installed in the base image (extracting the packages by running the image with `stevedore` [@stevedore]) at the cost of expressiveness.

The package `containerit`'s main contribution the capturing of runtime environments as `Dockerfile`s based on literate programming workflows [@gentleman_statistical_2007] encouraged for reproducible research.
It collects all instructions to recreate an R environment in a single `Dockerfile`.
Together with `stevedore` [@stevedore] it enables a completely R-based creation and manipulation of of Docker containers.
The impact on researcher's workflow using `containerit` is small because it can be applied after completing a workflow, but the captured snapshots can enhance the scholarly publication process (in particular review, interaction, and preservation) and may form a basis for more re-usable and transparent publications.

In the future `containerit` may support alternative container software, such as Singularity [@kurtzer_singularity_2017], enable parametrisation of container executions and pipelines as demonstrated by Kliko [@molenaar_klikoscientific_2018], or support proper creditation of software [@codemeta, @katz_software_2018].

# Related work

Other tools to automatically create a container for a portable and reproducible runtime environment in research applications exist, though none of them applies the strict code execution approach as `containerit` does, and not all of them only create a `Dockerfile` while some are coupled with a specific workflow or toolset.
`automagic` [@automagic], [Whales](https://github.com/Gueils/whales), [`dockter`](https://github.com/stencila/dockter/), and 
[`repo2docker`](https://github.com/jupyter/repo2docker) use using static program analysis to create environment descriptions.
The first analyses R code and can store dependencies in a bespoke [YAML](https://en.wikipedia.org/wiki/YAML) format.
The next two provide different formats, including `Dockerfile`.
The latter three use common project configuration files for multiple programming languages.
The last one primarily creates a container for interactive notebooks [powering Binder, see @jupyter2018binder] but does not actively expose a `Dockerfile`.
[ReproZip](https://www.reprozip.org/) [@ChirigatiRSF16] packages files identified by [tracing](https://en.wikipedia.org/wiki/Tracing_(software)) in a self-contained bundle, which can be unpacked to amongst others a Docker container without a `Dockerfile` exposed.

In the R domain, the package `dockerfiler` [@fay_dockerfiler] provides an object-oriented API for manual Dockerfile creation, and `lifr` [@xiao_liftr] creates a `Dockerfile` based on fields added to the metadata header of an R Markdown document.
[The Experiment Factory](https://expfactory.github.io/) similarly focuses on easy of use for creating `Dockerfile`s for behavourial experiments, yet it uses a CLI-based interaction and generates extra shell scripts to be included in the images.

# Acknowledgements

This work is supported by the project [Opening Reproducible Research](https://o2r.info) ([Offene Reproduzierbare Forschung](https://www.uni-muenster.de/forschungaz/project/9520)) funded by the German Research Foundation ([DFG](http://dfg.de/)) under project number `PE 1632/10-1`.

# References
