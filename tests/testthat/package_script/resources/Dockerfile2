FROM rocker/r-ver:3.3.2
LABEL maintainer="o2r"
WORKDIR /payload/
COPY ["package_script/resources/simple_test.R", "package_script/resources/simple_test.R"]
COPY ["package_script/resources/test_table.csv", "package_script/resources/test_table.csv"]
COPY ["package_script/resources/test_subfolder/testresource", "package_script/resources/test_subfolder/testresource"]
CMD ["R"]
