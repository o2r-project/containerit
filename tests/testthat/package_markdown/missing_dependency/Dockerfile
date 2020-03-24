FROM rocker/r-ver:3.4.4
LABEL maintainer="o2r"
RUN ["install2.r", "coxrobust", "rprojroot", "survival"]
WORKDIR /payload/
CMD ["R"]
