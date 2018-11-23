interactive_addin = function() {
  sys.source(pkg_file('scripts','package-interactive-session.R'))
}

fromfile_addin = function() {
  sys.source(pkg_file('scripts','package-load-file.R'))
}