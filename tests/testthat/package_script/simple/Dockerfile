FROM rocker/r-ver:3.4.3
LABEL maintainer="o2r"
RUN ["install2.r", "magrittr", "rjson", "stringi", "stringr"]
WORKDIR /payload/
COPY ["package_script/simple_test.R", "package_script/simple_test.R"]
CMD ["R"]
