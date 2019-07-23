.pkg_file = function(..., mustWork = TRUE) {
  system.file(..., package = 'containerit', mustWork = mustWork)
}

#' Helper function for RStudio Addin user interface
#'
#' See \code{README.Rmd} for details about the RStudio Addin.
#' roxygen comment mainly to configure required imports for UI in one place.
#'
#' @name containerit-addin
#'
#' @importFrom miniUI miniPage gadgetTitleBar miniContentPanel
#' @importFrom shinyFiles shinyFileSave shinyFilesButton shinyFileChoose parseFilePaths shinySaveButton parseSavePath
#' @import shiny
NULL

common_addin <- function() {
  sys.source(.pkg_file('scripts','common.R'))
}

fromSession_addin <- function() {
  common_addin()
  sys.source(.pkg_file('scripts','package-session.R'))
}

fromFile_addin <- function() {
  common_addin()
  sys.source(.pkg_file('scripts','package-file.R'))
}

fromExpressions_addin <- function() {
  common_addin()
  sys.source(.pkg_file('scripts','package-expressions.R'))
}

fromWorkspace_addin = function() {
  common_addin()
  sys.source(.pkg_file('scripts','package-workspace.R'))
}
