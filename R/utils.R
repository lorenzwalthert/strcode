`%AND%` <- function(x, y) {
  if (!is.null(x) && !is.na(x))
    if (!is.null(y) && !is.na(y))
      return(y)
  return(NULL)
}


#'insert text with autofocus
#'
#'# function is identical to textAreaInput but adds autofocus to tags
#' @inheritParams shiny::textAreaInput
text_focus <- function (inputId, label, value = "", width = NULL, height = NULL,
                        cols = NULL, rows = NULL, placeholder = NULL, resize = NULL)
{
  value <- restoreInput(id = inputId, default = value)
  if (!is.null(resize)) {
    resize <- match.arg(resize, c("both", "none", "vertical",
                                  "horizontal"))
  }
  style <- paste(if (!is.null(width))
    paste0("width: ", validateCssUnit(width), ";"), if (!is.null(height))
      paste0("height: ", validateCssUnit(height), ";"), if (!is.null(resize))
        paste0("resize: ", resize, ";"))
  if (length(style) == 0)
    style <- NULL
  div(class = "form-group shiny-input-container", label %AND%
        tags$label(label, `for` = inputId),
      tags$textarea(id = inputId,
                    class = "form-control", placeholder = placeholder, style = style,
                    rows = rows, cols = cols, value,
                    autofocus = "autofocus"))
}


#   ____________________________________________________________________________
#   testing helpers                                                         ####

#' check via reading files
#'
#' check whether a writen correct file is equivalent to a writen one
#' produced by strcode
#' @param directory A path relative to "tests/testthat"
#' @param filename The name of a file to check. The name of the file to check
#'   against is \code{filename_correct}
#' @param path_start A string specifiing the location of of testthat relative
#'   to the project root. See details
#'
#' @details When testing is performed manually (that is, not via test routine
#'   of RStudio), reading in files needs a directory relative to the project
#'   main directory. When running tests via RStudio, the directory must be
#'   indicated relative to the testthat folder. Hence, for manual testing,
#'   we need a different path than for routine testing. The problem can be
#'   solved by assigning a global variable to path_start manually plus setting
#'   path_start to \code{NULL} at the beginning of the document, so when the
#'   tests are run via the RStudio routine, they are set correctly.
#'
check_via_read <- function(directory = "test-dir_in",
                           filename = "code_summary-example-1",
                           path_start) {
  filenames <- c(filename, paste0(filename, "_correct"))
  path <- ifelse(is.null(path_start), paste(directory, filenames, sep = "/"),
                 paste(path_start, directory, filenames, sep = "/"))

  # read files in
  compare <- lapply(path, readLines)
  length(unique(compare)) == 1
}

