interactive_addin = function() {
  sys.source(pkg_file('scripts','package-interactive-session.R'))
}

fromfile_addin = function() {
  sys.source(pkg_file('scripts','package-load-file.R'))
}

fromexpressions_addin = function() {
  sys.source(pkg_file('scripts','package-expressions.R'))
}

workspaceDirectory_addin = function() {
  sys.source(pkg_file('scripts','package-workspace-directory.R'))
}