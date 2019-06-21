FROM rocker/r-ver:3.3.2
LABEL maintainer="o2r"
RUN ["install2.r", "magrittr", "rjson", "stringi", "stringr"]
WORKDIR /payload/
COPY ["package_script/simple_lowercase/", "package_script/simple_lowercase/"]
CMD ["R", "--vanilla", "-f", "package_script/simple_lowercase/simple_test.r"]
