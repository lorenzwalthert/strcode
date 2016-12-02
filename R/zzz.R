.onLoad <- function(libname, pkgname) {
  op <- options()
  op.strcode <- list(
    strcode.char.length = 80
  )
  toset <- !(names(op.strcode) %in% names(op))
  if(any(toset)) options(op.strcode[toset])

  invisible()
}
