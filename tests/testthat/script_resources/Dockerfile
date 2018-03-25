FROM rocker/r-ver:3.3.2
LABEL maintainer="matthiashinz"
WORKDIR /payload/
COPY ["script_resources", "script_resources/"]
CMD ["R", "--vanilla", "-f", "script_resources/simple_test.R"]
