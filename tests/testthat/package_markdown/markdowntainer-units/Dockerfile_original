FROM rocker/tidyverse:3.3.2
# user tidyverse so have littler install scripts and use MRAN
MAINTAINER Daniel Nüst <daniel.nuest@uni-muenster.de>

RUN apt-get update -qq \
	&& apt-get install -y --no-install-recommends \
	## Packages required by R extension packages
	# required by rmarkdown:
	lmodern \
	pandoc \
	# for devtools (requires git2r, httr):
	libcurl4-openssl-dev \
	libssl-dev \
	git \
	# for udunits:
	libudunits2-0 \
	libudunits2-dev \
	# required when knitting the document
	pandoc-citeproc \
	&& apt-get autoremove -y \
	&& apt-get autoclean -y \
	&& rm -rf /var/lib/apt/lists/*

# install R extension packages
RUN install2.r -r "http://cran.rstudio.com" \
	  rmarkdown \
	  ggplot2 \
	  devtools \
	  && rm -rf /tmp/downloaded_packages/ /tmp/*.rd
RUN installGithub.r edzer/units

# Save installed packages to file
RUN dpkg -l > /dpkg-list.txt

LABEL Description="This is an ERC image." \
	info.o2r.bag.id="TWRgBfBT87"

COPY . /erc

ENTRYPOINT ["sh", "-c"]
CMD ["R --vanilla -e \"rmarkdown::render(input = '/erc/2016-09-29-plot_units.Rmd', output_format = rmarkdown::html_document())\""]

# CMD on cli:
# R --vanilla -e "rmarkdown::render(input = '2016-09-29-plot_units.Rmd', output_format = rmarkdown::md_document(variant = 'markdown_github', preserve_yaml = TRUE))"
# R --vanilla -e "rmarkdown::render(input = '2016-09-29-plot_units.Rmd', output_format = rmarkdown::html_document())"

# docker build --tag markdowntainer-units .
# docker run -it markdowntainer-units