FROM rocker/tidyverse:3.3.2
# user tidyverse so have littler install scripts and use MRAN
MAINTAINER Daniel Nüst <daniel.nuest@uni-muenster.de>

RUN apt-get update -qq \
	&& apt-get install -y --no-install-recommends \
	wget \
	build-essential \
	## Packages required by R extension packages
	# for rmarkdown:
	lmodern \
	pandoc \
	# for devtools (requires git2r, httr):
	libcurl4-openssl-dev \
	libssl-dev \
	git \
	# for rgeos:
	libgeos-3.4.2 \
	libgeos-dev \
	# required when knitting the document
	pandoc-citeproc \
	# clean up apt
	&& apt-get clean \
	&& rm -rf /var/lib/apt/lists/*

# install latest GDAL and PROJ, based on https://github.com/edzer/sfr/blob/master/.travis.yml
WORKDIR /tmp/gdal
RUN wget http://download.osgeo.org/gdal/2.1.0/gdal-2.1.0.tar.gz \
	&& tar zxf gdal-2.1.0.tar.gz \
	&& cd gdal-2.1.0 \
	&& ./configure \
	&& make \
	&& make install
WORKDIR /tmp/proj
RUN wget http://download.osgeo.org/proj/proj-4.9.3.tar.gz \
	&& tar zxvf proj-4.9.3.tar.gz \
	&& cd proj-4.9.3 \
	&& ./configure \
	&& make \
	&& make install \
	&& ldconfig
RUN rm -r /tmp/gdal /tmp/proj

# install R extension packages
RUN install2.r -r "http://cran.rstudio.com" \
	  rmarkdown \
	  devtools \
	  rgeos \
	  maptools \
	  && rm -rf /tmp/downloaded_packages/ /tmp/*.rd
RUN installGithub.r edzer/rgdal2

# install specific version of package to be demonstrated
RUN git clone https://github.com/edzer/sfr.git /sfr \
	&& cd /sfr \
	&& git reset --hard 16e205f54976bee75c72ac1b54f117868b6fafbc \
	&& R --vanilla -e "devtools::install('/sfr')"

# save installed packages to file
RUN dpkg -l > /dpkg-list.txt

LABEL Description="This is an ERC image." \
	info.o2r.bag.id="332ed55649c2"

COPY . /erc
WORKDIR /erc

ENTRYPOINT ["sh", "-c"]
CMD ["R --vanilla -e \"rmarkdown::render(input = '/erc/2016-07-17-sf2.Rmd', output_format = rmarkdown::html_document())\""]

# CMD on cli:
# R --vanilla -e "rmarkdown::render(input = '2016-07-17-sf2.Rmd', output_format = rmarkdown::html_document())"

# docker build --tag markdowntainer-sfr .
# docker run -it markdowntainer-sfr