pkg_file = function(..., mustWork = TRUE) {
  system.file(..., package = 'containerit', mustWork = mustWork)
}