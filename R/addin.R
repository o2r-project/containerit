common_addin <- function() {
  sys.source(pkg_file('scripts','common.R'))
}

fromSession_addin <- function() {
  common_addin()
  sys.source(pkg_file('scripts','package-interactive-session.R'))
}

fromFile_addin <- function() {
  common_addin()
  sys.source(pkg_file('scripts','package-load-file.R'))
}

fromExpressions_addin <- function() {
  common_addin()
  sys.source(pkg_file('scripts','package-expressions.R'))
}

workspaceDirectory_addin = function() {
  sys.source(pkg_file('scripts','package-workspace-directory.R'))
}
