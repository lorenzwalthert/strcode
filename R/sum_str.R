#' Summarize the code structure
#'
#' Create a summary of one or multiple code files by collapsing them into the
#' section separators and their comments.
#' @param dir_in A directory where the file(s) can be found.
#' @param dir_out A directory where to print the output to. "" implies the
#'   console.
#' @param file_in The name of a file which should be summarized. \code{NULL} will
#'   create a summary of all files in the directory.
#' @param file_in_extension If \code{file_in} is \code{NULL}, all files with the
#'   \code{file_in_extension} are considered.
#' @param file_out A connection or character string naming the file to print to.
#'   The argument is irrelevant if \code{output_dir} is set to "".
#' @param file_out_extension An file extension for the file to be created.
#' @param width The character width of the output.
#' @param line_nr A boolean value that indicates whether the line numbers should
#'   be printed along with the structure summary.
#' @param separator A boolean value indicating whether the separating lines
#'   should be reported along their comments or not.
#' @param title A boolean value indicating whether the reported summary should
#'   contain a title or not.
#' @param ... futher arguments to be passed from and to other methods, in
#'   particular \code{\link{list.files}} for reading in multiple files.
#'
#'
#'
#'
#' @export
#'
#   ____________________________________________________________________________
#   user-function
sum_str <- function(dir_in = "./vignettes/",
                    dir_out = dir_in,
                    file_in = "example.R",
                    file_in_extension = ".R",
                    file_out = NULL,
                    file_out_extension = "",
                    width = 50,
                    line_nr = TRUE,
                    separator = TRUE,
                    title = TRUE,
                    ...) {

##  ............................................................................
##  prepare input to call helper repeated times.
if (is.null(file_in)) {
  all_files <- as.list(list.files(path = dir_in,
                          pattern = paste0(file_in_extension, "$"),
                          full.names = FALSE,
                          ...)
  )

} else {
  all_files <- as.list(file_in)
}

##  ............................................................................
##  call helper
  lapply(all_files, function(g) {
    sum_str_helper(dir_in = dir_in,
                   dir_out = dir_out,
                   file_in = g,
                   file_out = file_out,
                   file_out_extension = file_out_extension,
                   width = width,
                   line_nr = line_nr,
                   separator = separator,
                   title = title)
  })
  cat("The following files were summarized",
          as.character(all_files), sep = "\n")
}

#   ____________________________________________________________________________
#   helper function

sum_str_helper <- function(dir_in,
                           dir_out,
                           file_in,
                           file_out,
                           file_out_extension,
                           width,
                           line_nr,
                           separator,
                           title) {
##  ............................................................................
## argument interaction
  if (is.null(file_out)) {
    file_out <- paste0("code_summary-",
                       gsub("^(.*)\\..*$", "\\1", file_in, perl = TRUE),
                       file_out_extension)
  }

##  ............................................................................
##  get pattern

  path <- paste0(dir_in, file_in)
  lines <- readLines(con = path)
  pos <- grep("^#+([[:space:]]){1,4}", lines) # extract candiates
  # allow spaces in the beginning (deactivated)
  # pos <- grep("^[[:space:]]*#+([[:space:]]){1,4}", lines)
  pattern <-lines[pos]

##  ............................................................................
##  modify pattern according to arguments
  if (separator == FALSE) {
    remove <- grep("^#+\\s+[_|\\.|\\..\\s]+$", lines[pos], perl = TRUE)
    pattern <- pattern[-remove]
  }


  if (!is.null(width)) {
    pattern <- substring(pattern, 1, width)
  }

  if (line_nr == TRUE) {
    pattern <- paste(pos, pattern, sep = "\t")
  }

  if (title == TRUE) {
    pattern <- append(paste0("Table of contents for ", file_in), pattern)
  }

  if ("" %in% c(dir_out, file_out)) {
    path_out <- ""
  } else {
    path_out <- paste(dir_out, file_out, sep = "/")
  }


##  ............................................................................
##  output pattern
  cat(pattern, file = path_out, sep = "\n")
}

# relace ^\s+(#+) with \1 in Rstudio to move all breaks to the left.
# extensions
# - multiple files in a directrory
# - output to file



