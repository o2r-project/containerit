# Copyright 2018 Opening Reproducible Research (https://o2r.info)

#' Class union for slots taking either numeric or character input
#'
#' @return object
#' @export
setClassUnion("characterOrInteger", c("character", "integer"))
