.onLoad <- function(libname, pkgname) {
  op <- options()
  op.strcode <- list(
    strcode.char.lenght = 80,
    strcode.section.title = FALSE
  )
  toset <- !(names(op.strcode) %in% names(op))
  if(any(toset)) options(op.strcode[toset])

  invisible()
}

