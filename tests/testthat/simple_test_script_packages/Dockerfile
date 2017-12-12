FROM rocker/r-ver:3.4.3
LABEL maintainer="o2r"
RUN ["install2.r", "magrittr", "rjson", "stringi", "stringr"]
WORKDIR /payload/
COPY ["simple_test_script_packages/simple_test.R", "simple_test_script_packages/simple_test.R"]
CMD ["R"]
