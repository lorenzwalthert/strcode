.onLoad <- function(libname, pkgname) {
  op <- options()
  op.strcode <- list(strcode = list(
    char_length = 80,
    insert_with_shiny = TRUE,
    hash_in_sep = TRUE
  )
  )
  toset <- !(names(op.strcode) %in% names(op))
  if(any(toset)) options(op.strcode[toset])

  invisible()
}

