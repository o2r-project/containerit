FROM rocker/verse:3.5.2
LABEL maintainer="Ted Tester"
RUN export DEBIAN_FRONTEND=noninteractive; apt-get -y update \
  && apt-get install -y libudunits2-dev
RUN ["install2.r", "ggplot2", "units"]
WORKDIR /payload/
COPY ["package_markdown/units/", "package_markdown/units/"]
CMD ["R", "--vanilla", "-e", "rmarkdown::render(input = \"package_markdown/units/2016-09-29-plot_units.Rmd\", output_format = rmarkdown::html_document())"]
