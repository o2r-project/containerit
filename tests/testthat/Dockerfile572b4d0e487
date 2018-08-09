FROM rocker/r-ver:3.3.2
LABEL maintainer="o2r"
WORKDIR /payload/
COPY ["package_script/resources/", "package_script/resources/"]
CMD ["R", "--vanilla", "-f", "package_script/resources/simple_test.R"]
