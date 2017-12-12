# Copyright 2017 Opening Reproducible Research (http://o2r.info)

#' Class union for slots taking either numeric or character input
#'
#' @return object
#' @export
setClassUnion("characterOrInteger", c("character", "integer"))
