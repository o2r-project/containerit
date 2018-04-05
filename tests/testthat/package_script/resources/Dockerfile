FROM rocker/r-ver:3.4.3
LABEL maintainer="o2r"
WORKDIR /payload/
COPY ["package_script/resources/simple_test.R", "package_script/resources/simple_test.R"]
CMD ["R", "--vanilla", "-f", "package_script/resources/simple_test.R"]
