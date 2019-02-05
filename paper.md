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
However, especially domain scientists struggle with the complexity of managing and capturing their computational environment in containers.
The software `containerit` assists researchers in capturing workflows and documents for R [@r_2018].
While computer scientists and software developers are often capable to create and manage containers by hand, the automatic generation of containers using only R commands opens up the advantages of containers to a much larger user base.

# Containers for Science

Originally created for isolation of software and assigning specific operating system resources, containerisation took off when as a technology for packaging server applications and their dependencies for fast, scalable, and secure deployments in cloud-based infrastructures (cf. @osnat_brief_2018).
Containers can enable most the advantages that virtulisation and cloud computing can bring to computational science, such as capturing digital laboratories of in-silico experiments, capturing dependencies without constraints on research methods, packaging and re-depoy of complex architectures, and new approaches to handling costs of hosting scientific results [@howe_virtual_2012].

The core building block of a container-based infrastructure is the _image_, which can be executed in one or many instances, the _container_, using a _container runtime_, and moved between systems as files (e.g. image tarballs) or based on an _image registry_.
An important advantage of a containers compared to a [virtual machine](https://en.wikipedia.org/wiki/Virtual_machine) is the duality between the recipe for creating a container and the image that is build with that recipe.
While manual alteration of containers are possible, it is a best practice to automate all configuration.
The most widely used containerisation software is [Docker](https://en.wikipedia.org/wiki/Docker_(software)).
The recipees with instructions what to include in an image is the `Dockerfile`.

For scholarly applications, portability, transparency, and the reduction to a single runtime dependency, i.e. the container runtime, are more important than security (e.g. [sandbox](https://en.wikipedia.org/wiki/Sandbox_(computer_security))).
The same virtual laboratory that a researcher uses locally, including system dependencies etc., can be packaged, moved, and easily re-opened by other researchers on their machines or even on cloud-based infrastructures [@jupyter2018binder].
This way containers have been shown to enable verification of reproducibility and auditing without requiring reviewers to manually download, install, and re-run analyses [@beaulieu-jones_reproducibility_2017].
Running the analysis within a container greatly increases trust in the stability of a workflow as it can mimic a second, independently conducted execution of the code.
The `Dockerfile` and image may be published alongside a scientific paper to support peer review and to some extend archival [@nust_opening_2017].
Container preservation is an active field of research [@rechert_preserving_2017; @emsley_framework_2018], although it is reasonable to assume that the operation of a container runtime at time scales comparable to data storage requirements by funding agencies, e.g. 10 years in case of the [German DFG](http://www.dfg.de/en/research_funding/proposal_review_decision/applicants/research_data/index.html)), is possible for universities or scholarly publishers.
Even if an image cannot be executed, or a `Dockerfile` cannot be build anymore, the instructions in the `Dockerfile` are human-readable and allow to fix a problem or to recreate an environment from scratch that closely resembles the original environment.

# The package `containerit`

- uses rocker images [@RJ-2017-065]
- r-ver stack enables snapshotting of cran and is best for reproducibility
- executes the analysis in a new R session
- captures interactive sessions, and streamlines the capturing of the session information for R scripts and R Markdown [@allaire_rmarkdown_2018] documents.

Other than similar packages (see below), `containerit` will only uses [static program analysis](https://en.wikipedia.org/wiki/Static_program_analysis) for preparing a blank capturing environment e.g. when creating Dockerfiles for R Markdown documents as a service [@nust_reproducibility_2018].
Static program analysis can be broken by side-effects, seemingly clever ways to load packages, or using helper functions such as `xfun::pkg_attach()` [@xie_xfun_2018].

- in combination with stevedore even image creation and container manipulation are possible using only R functions
- exposes key functions, such as exposing ports, because Docker does not provide a [display server](https://en.wikipedia.org/wiki/Display_server) that would allow to run applications with  [graphical user interface](https://en.wikipedia.org/wiki/Graphical_user_interface) (GUI), because Docker it is originally built for server software.
The common way to provide is by providing a web server and rendering an HTML-based GUI in a common web browser, e.g. as notebooks [@jupyter2018binder].

`containerit` is not about standardisation of pipelines, but about capturing snapshots of research publications, because while Open Science has a great impact on the way researchers work, the generic scholarly publication process (and research funding schemes) of doing resarch and at a certain point in time publishing the results is not going to disappear any time soon.
It has a limited impact on the workflow, which only must be based on scripts or documents following the literate programming paradigm [@gentleman_statistical_2007], such as R Markdown.

- "using" software and continuously porting it is of course more useful (cf. CRAN system of packages that encourages constant maintenance), but challenging in a scholarly context
- proper creditation/citing of all software in a container not yet solved (cite codemeta??)
- parametrisation of workflows

In the future `containerit` may support alternative container software, such as Singularity [@kurtzer_singularity_2017], or enable parametrisation of container executions and pipelines as demonstrated by Kliko [@molenaar_klikoscientific_2018].

# Related work

- `automagic`
- GUIdock
- `repo2docker`
- `dockter`
- ReproZip
- Experiment factory, user interface for generation of environment
- s2i (source-to-image), http://words.yuvi.in/post/why-not-s2i/
- Kliko [@molenaar_klikoscientific_2018]

# Acknowledgements

This work is supported by the project [Opening Reproducible Research](https://o2r.info) ([Offene Reproduzierbare Forschung](https://www.uni-muenster.de/forschungaz/project/9520)) funded by the German Research Foundation ([DFG](http://dfg.de/)) under project number `PE 1632/10-1`.

# References
