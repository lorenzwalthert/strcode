#' Summarize the code structure
#'
#' Create a summary of one or multiple code files by collapsing them into the
#' section separators and their comments.
#' @param dir_in A directory where the file(s) can be found.
#' @param dir_out A directory where to print the output to. "" implies the
#'   console.
#' @param file_in The name of a file which should be summarized. \code{NULL} will
#'   create a summary of all files in the directory. The default value uses the
#'   RStudio api and produces a summary of the source editor.
#' @param file_in_extension If \code{file_in} is \code{NULL}, all files with the
#'   \code{file_in_extension} are considered.
#' @param file_out A connection or character string naming the file to print to.
#'   The argument is irrelevant if \code{output_dir} is set to "".
#' @param file_out_extension An file extension for the file to be created.
#' @param width The character width of the output.
#' @param line_nr A boolean value that indicates whether the line numbers should
#'   be printed along with the structure summary.
#' @param granularity Indicating how many levels should be in the summary
#' @param lowest_sep A boolean value indicating whether the separating lines
#'   should be reported along their comments or not.
#' @param title A boolean value indicating whether the reported summary should
#'   contain a title or not.
#' @param header A boolean values indicating whether a column header should
#'   indicate the name of the columns (line, level, section).
#' @param ... futher arguments to be passed from and to other methods, in
#'   particular \code{\link{list.files}} for reading in multiple files.
#' @examples
#' \dontrun{
#' # open a new .R file in Rstudio, insert some code breaks
#' # using the Add-in of this package, safe the file and run:
#' sum_str() # get a summary of the source editor.
#' }
#' @importFrom rstudioapi getSourceEditorContext
#' @export
#'
#   ____________________________________________________________________________
#   user-function
sum_str <- function(dir_in = NULL,
                    dir_out = "",
                    file_in = getSourceEditorContext()$path,
                    file_in_extension = ".R",
                    file_out = NULL,
                    file_out_extension = "",
                    width = 50,
                    line_nr = TRUE,
                    granularity = 3,
                    lowest_sep = TRUE,
                    title = TRUE,
                    header = TRUE,
                    ...) {

##  ............................................................................
##  prepare input to call helper repeated times.
  # in the case there are multiple files
  if (is.null(file_in)) {
    all_files <- as.list(list.files(path = dir_in,
                            pattern = paste0(file_in_extension, "$"),
                            full.names = FALSE,
                            ...)
    )

  # in the case there is just one file
  } else {
    all_files <- as.list(file_in)
  }

##  ............................................................................
##  call helper
  lapply(all_files, function(g) {
    # pass all arguments as is except the file_in
    sum_str_helper(dir_in = dir_in,
                   dir_out = dir_out,
                   file_in = g,
                   file_out = file_out,
                   file_out_extension = file_out_extension,
                   width = width,
                   line_nr = line_nr,
                   granularity = granularity,
                   lowest_sep = lowest_sep,
                   title = title,
                   header = header)
  })

  # if output is not printed in the console, print a short summary.
  if (dir_out != "") {
    cat("The following files were summarized",
            as.character(all_files), sep = "\n")
  }
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
                           granularity,
                           lowest_sep,
                           title,
                           header) {
##  ............................................................................
##  argument interaction

### .. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
### get the file_out together
if (is.null(file_out)) {
  file_out <- paste0("code_summary-",
                     gsub("^(.*)\\..*$", "\\1", file_in, perl = TRUE),
                     file_out_extension)
}



### .. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
### paths
  # path_in
  ## note that file_in is never null when called from sum_str()

  # if dir is not null, the path is composed of dir and file
  if (!is.null(dir_in)) {
    path_in <- paste(dir_in, file_in, sep = "/")
  # otherwise it is simply the file_in
  } else {
    path_in <- file_in
  }

  # path_out
  ## path_out is "" if dir_out is ""
  if (is.null(dir_out) || dir_out == "") {
    path_out <- ""
  # otherwise it is composed of dir_out and file_out, if file_out
  # has a not empty value
  } else {
    path_out <- paste(dir_out, file_out, sep = "/")
  }



##  ............................................................................
## function definitions
  # find maximal or minimal level of granularity used.
  find_gran <- function(direction = "up") {
    if (direction == "up") {
      l <- 1 # initialize
    } else if (direction == "down") {
      l <- 3
    }

    helper_find_gran <- function(direction) {
      if (direction == "up") {
        m <- 1
      } else if (direction == "down") {
        m <- -1
      }
      pattern <- paste0("^", paste0(rep("#", l), sep = "", collapse = ""), "\\s+")
      if (any(grepl(pattern, lines, perl = TRUE))) {
        l

      } else {
        l <<- l + m * 1
        helper_find_gran(direction = direction)
      }
    }

    helper_find_gran(direction)
  }

##  ............................................................................
##  get pattern

  lines <- readLines(con = path_in)
  sub_pattern <- "^#+([[:space:]]){1,4}"
  pos <- grep(sub_pattern, lines) # extract candiates
  # allow spaces in the beginning (deactivated)
  # pos <- grep("^[[:space:]]*#+([[:space:]]){1,4}", lines)
  pattern <-lines[pos]

##  ............................................................................
##  modify pattern according to arguments

### .. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
### getting the granularity right

  # remove the l lowest pattern separator depending on granularity if
  # lowest_sep is TRUE
  if (lowest_sep == FALSE) {
   sub_pattern <- paste0("^#{", min(granularity, find_gran("down")), ",", 4,
                          "}\\s+[_|\\.|\\..\\s]+$")
   remove <- grep(sub_pattern, lines[pos], perl = TRUE)
   pattern <- pattern[-remove]
   pos <- pos[-remove]
  }

  # removing the l lowest separator comments depending on granularity
  get_gran_pattern <- function(level = 3) {
    paste("^", "#", "{", 1, ",", level, "}",
          "\\s{", 1, ",", level, "}", sep = "", collapse = "")
  }

  update_pos_pattern <- function(level) {
    keep <- grep(get_gran_pattern(level = level), lines[pos], perl = TRUE)
    pattern <<- pattern[keep]
    pos <<- pos[keep]

  }

  update_pos_pattern(granularity)

##  ............................................................................
## width adjust line_nr, title, output path, header
  if (!is.null(width)) {
    pattern <- substring(pattern, 1, width)
  }

  if (line_nr == TRUE) {
    pattern <- paste(pos, pattern, sep = "\t")
  }

  if (header == TRUE) {
    pattern <- append(c("line  level section"), pattern)
  }
  if (title == TRUE) {
    pattern <- append(paste0("Summarized structure of ", file_in, "\n"), pattern)
  }



##  ............................................................................
##  output the pattern
  cat(pattern, file = path_out, sep = "\n")
}

 # relace ^\s+(#+) with \1 in Rstudio to move all breaks to the left.
 # extensions
 # - multiple files in a directrory
 # - output to file



