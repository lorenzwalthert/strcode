.onLoad <- function(libname, pkgname) {
  op <- options()
  op.strcode <- list(strcode = list(
    char_length = 80
  )
  )
  toset <- !(names(op.strcode) %in% names(op))
  if(any(toset)) options(op.strcode[toset])

  invisible()
}

