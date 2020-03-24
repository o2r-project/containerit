FROM rocker/r-ver:3.3.2
LABEL maintainer="o2r"
RUN export DEBIAN_FRONTEND=noninteractive; apt-get -y update \
  && apt-get install -y git-core
RUN ["install2.r", "remotes"]
RUN ["installGithub.r", "r-hub/sysreqs@3860f2b512a9c3bd3db6791c2ff467a1158f4048"]
WORKDIR /payload/
CMD ["R"]
