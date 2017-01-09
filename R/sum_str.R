#' Summarize the code structure
#'
#' Create a summary of one or multiple code files based on the section
#' separators and their comments.
#' @param dir_in The directory where the file(s) can be found.
#' @param file_in The name of a file which should be summarized. If this is
#'   \code{NULL}, the summary will be for all files in the specified directory.
#'   The default value uses the RStudio API to produce a summary of content from
#'   the source editor. This requires that
#'   the file is saved before \code{sum_str} is called.
#' @param file_in_extension If \code{file_in} is \code{NULL}, all files with the
#'   \code{file_in_extension} are considered, defaults to ".R".
#' @param dir_out The directory to print the output to. "" implies the console.
#' @param file_out A connection or character string naming the file to print to.
#'   If set to \code{NULL}, the name will be \code{paste0("code-summary_", file_in)}.
#'   If \code{dir_out} is set to "", \code{file_out} can be set to
#'   "object" and the output of the function will be returned as an
#'   object instead of just printed to the console with \code{cat}. This is
#'   useful if output should be assigned to an object. If not set to "object",
#'   \code{cat} will be used.
#' @param file_out_extension A file extension for the file to be created.
#' @param width The character width of the output. If NULL, it is set to the
#'   length of the longest separator comment.
#' @param line_nr A boolean value that indicates whether the line numbers should
#'   be printed along with the structure summary.
#' @param granularity Indicates the lowest level of granularity that should be
#'   included in the summary.
#' @param lowest_sep A boolean value indicating whether or not the separating
#'   lines of the lowest level should be printed.
#' @param title A boolean value indicating whether the reported summary should
#'   contain a title or not.
#' @param header A boolean value indicating whether a column header should
#'   indicate the name of the columns (line, level, section).
#' @param ... futher arguments to be passed from and to other methods, in
#'   particular \code{\link{list.files}} for reading in multiple files.
#' @details To create the summary, \code{sum_str} uses regular expressions.
#'   Hence it is crucial that the code separators and the separator comments
#'   match the regular expression pattern. We recommend inserting
#'   separators and their comments using the RStudio Add-in that is contained
#'   in this package. The definition is rather intuitive as can be seen in the
#'   example section below. However, we shall provide a formal definition here
#'   as well.
#'   \itemize{
#'     \item A code separator is defined as a line that starts with n hashes,
#'     followed by 4-n spaces where 0 < n < 4. This sequence is followed by one
#'     or more either \code{.} or \code{_}.
#'     \item A comment associated with a code separator is defined as a line
#'     that starts with n hashes, followed by 4-n spaces where 0 < n < 4. This
#'     sequence is \emph{not} followed by \code{.} or \code{_}.
#'   }
#'   Lines that do not satisfy these requirements (e.g. do not start with #s,
#'   do not contain the right number of spaces after the #, indent before any #
#'   ect.) are not considered by \code{sum_str}.
#'
#' @examples
#' # the following separators are examples of valid
#' # separators and associated comments
#'
#' #   __________________________________________________
#' #   this is a level 1 comment
#' ##  . . . . . . . . . . . . . . . . . . . . . . . . .
#' ##  note that the comment or the separator character (_, .)
#' ##  always starts at indention 4.
#'
#' \dontrun{
#' # Open a new .R file in RStudio, insert some code breaks
#' # using the Add-in of this package, save the file and run:
#' sum_str() # get a summary of the source editor.
#' }
#' @importFrom rstudioapi getSourceEditorContext
#' @export
#'
#   ____________________________________________________________________________
#   user-function
sum_str <- function(dir_in = NULL,
                    file_in = getSourceEditorContext()$path,
                    file_in_extension = ".R",
                    dir_out = "",
                    file_out = NULL,
                    file_out_extension = "",
                    width = NULL,
                    line_nr = TRUE,
                    granularity = 3,
                    lowest_sep = TRUE,
                    title = TRUE,
                    header = TRUE,
                    ...) {
##  ............................................................................
##  assertive test

assert_number(granularity, lower = 1, upper = 3)

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

  # if output is not printed in the console, print a short summary.
  if (dir_out != "") {
    cat("The following files were summarized \n")
  }
  output <- lapply(all_files, function(g) {
    # pass all arguments as is except the file_in
    if (dir_out != "") {
      cat(g, sep = " \n")
    }
    sum_str_helper(dir_in = dir_in,
                   file_in = g,
                   dir_out = dir_out,
                   file_out = file_out,
                   file_out_extension = file_out_extension,
                   width = width,
                   line_nr = line_nr,
                   granularity = granularity,
                   lowest_sep = lowest_sep,
                   title = title,
                   header = header)
  })

  if (dir_out == "" && !is.null(file_out) && file_out == "object") {
    output
  } else {
    invisible() # avoid unnecessary NULL return
  }
}

#   ____________________________________________________________________________
#   helper function: sum_str_helper
#' helper function for code summarisation
#'
#' Function is called by \code{sum_str()} and returns summary of one code file.
#' @inheritParams sum_str
#' @param dir_in The directory where the file can be found.
#' @param file_in The name of a file which should be summarized.
#'   The default value uses the RStudio API to produce a summary of content from
#'   the source editor. This requires that
#'   the file is saved before \code{sum_str} is called.
#' @details The core of the function is described best as follows: after a file
#' was read in and stored in a vector *lines* whereas each element describes a
#' line of code, the candidate lines (in the sense that they might be contained
#' in the summary) were evaluated, their indices stored in *cand*. Next,
#' various regex patterns are matched against *lines*. Then,
#' after all tests were executed, the variable *remove* contains all indices
#' that should be removed from *lines* before it is returned as the summary of
#' the code file. Hence, applying \code{setdiff(cand, remove)} contains
#' the subset of *lines* that we finally want to output.
#' @keywords internal
#' @import checkmate
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
    # file_out must be a file name. Grap the last expression after backslash.
    backslash_rm <- gsub("^.*/(.*)", "\\1", file_in, perl = TRUE)
    file_out <- paste0("code_summary-",
                       gsub("^(.*)\\..*$", "\\1", backslash_rm, perl = TRUE),
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
  if (dir_out == "") {
    path_out <- ""
  # otherwise it is composed of dir_out and file_out, if file_out
  # has a not empty value
  } else {
    path_out <- paste(dir_out, file_out, sep = "/")
  }

##  ............................................................................
##  get pattern

  lines <- readLines(con = path_in)
  sub_pattern <- "^#   |^##  |^### "
  cand <- grep(sub_pattern, lines, perl = FALSE) # extract candiates id
  if (length(cand) == 0) {
    return(warning("No line matching the required pattern",
                   call. = FALSE, immediate. = TRUE))
  }

##  ............................................................................
##  modify pattern according to arguments
### .. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
### getting the granularity right
  remove <- c()
  if (granularity < 3) { # if there are any lines to remove
    hashes <- (granularity + 1):3
    spaces <- 4 - hashes

    # this variable stores the indices of all lines that should be dropped.
    for (i in 1:length(hashes)) {
      sub_pattern <- paste0("^#{", hashes[i], "}\\s{", spaces[i], "}.*$")
      remove <- append(remove, grep(sub_pattern, lines, perl = TRUE))
    }
  }

### .. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
### remove lowest separator
  if (lowest_sep == FALSE) {
    hashes <- min(find_gran("down", lines = lines), granularity)
    spaces <- 4 - hashes
    sub_pattern <- paste0("^#{", hashes, "}\\s{", spaces, "}[\\._].*$")
    remove <- append(remove, grep(sub_pattern, lines, perl = TRUE))
  }

##  ............................................................................
##  select elements that "survived all tests"
  tokeep <- setdiff(cand, remove)
  lines <- lines[tokeep]

### .. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
### width adjust line_nr, title, output path, header

  # only continue if there is a valid pattern
  if (length(lines) == 0) {
    return(warning("No line matching the required pattern",
                   call. = FALSE, immediate. = TRUE))
  }
  # adjust length of pattern.
  if (is.null(width)) {
  # first calculate width. It is the length of the maximal comment string
  ## get the comment strings
    pattern_comments <- grep("^(#   |##  |### )[^\\._)].*$", lines, value = TRUE)
    width <- max(nchar(pattern_comments))
  }

  lines <- substring(lines, 1, width)

  if (line_nr == TRUE) {
    lines <- paste(tokeep, lines, sep = "\t")
  }

  if (header == TRUE) {
    lines <- append(c("line  level section"), lines)
  }
  if (title == TRUE) {
    lines <- append(paste0("Summarized structure of ", file_in, "\n"), lines)
  }

##  ............................................................................
##  output the pattern
  if (dir_out == "" && file_out == "object") {
    lines
  } else {
    cat(lines, file = path_out, sep = "\n")
  }
}

#   ____________________________________________________________________________
#   helper function: find_gran
# find maximal or minimal level of granularity used.
#' Find out granularity of a string vector
#'
#' This helper function takes a string (vector) as an input and searches for the
#' highest or lowest granularity. Granularity is defined in terms of a hash/
#' space sequence (see \code{\link{sum_str}}). The search is implemented using
#' a recursive approach.
#' @param direction either "up" or "down". Down means the algorithm starts with
#' the pattern containing a certain number of hashes (specified in
#' \code{highest}) and searches for a regex match. If it can't find one, it
#' reduces the number of hashes by one and searches again, until it finds a
#' pattern.
#' @param highest The highest level of granularity to search for.
#' @param lowest The lowest level of granularity to search for.
#' @param lines a character vector containing the lines to match against.
#' @examples
#'   strcode:::find_gran("down", highest = 3, lowest = 1, lines = c("##  .", "#   _"))
#' @keywords internal
find_gran <- function(direction = "down", highest = 3, lowest = 1, lines) {
  # direction
  if (direction == "up") {
    current <- lowest
    m <- 1
  } else if (direction == "down") {
    current <- highest
    m <- -1
  }

    helper_find_gran <- function(direction) {
    assert_number(current, lower = lowest, upper = highest)
    pattern <- paste0("^", paste0(rep("#", current), collapse = ""),
                      "\\s{", highest + 1 - current, "}.*$")
    if (any(grepl(pattern, lines, perl = TRUE))) {
      current
    } else {
      current <<- current + m * 1
      helper_find_gran(direction = direction)
    }
  }
  helper_find_gran(direction)
}



