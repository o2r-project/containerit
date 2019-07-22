common_addin <- function() {
  sys.source(pkg_file('scripts','common.R'))
}

fromSession_addin <- function() {
  common_addin()
  sys.source(pkg_file('scripts','package-session.R'))
}

fromFile_addin <- function() {
  common_addin()
  sys.source(pkg_file('scripts','package-file.R'))
}

fromExpressions_addin <- function() {
  common_addin()
  sys.source(pkg_file('scripts','package-expressions.R'))
}

fromWorkspace_addin = function() {
  common_addin()
  sys.source(pkg_file('scripts','package-workspace.R'))
}