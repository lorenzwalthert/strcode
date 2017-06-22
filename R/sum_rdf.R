#' Summarize the code structure
#'
#' Create a summary of one or multiple code files based on the section
#' separators and their titles.
#' @param path_in Either a path to a directory or to a single file. If it is
#'   a directory path, all files in that directory will be summarised. If it
#'   is a single file path, only the resepective file will be summarised.
#'   The default value uses the RStudio API to produce a summary of content from
#'   the source editor. This requires that the file is saved before
#'   \code{sum_rdf} is called.
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
#' @param rm_rh_hashes Boolean value indicating whether or not to remove
#'   righthand hashes in section titles for the summary
#'   (see section Removing spaces and hashes).
#' @param rm_rh_spaces Boolean value indicating whether or not to remove
#'   righthand spaces in section titles for the summary
#'   (see section Removing spaces and hashes).
#' @param rm_break_anchors Boolean value indicating whether or not the anchors
#'   inserted in code separators should be removed for the summary.
#' @param width The character width of the output. If NULL, it is set to the
#'   length of the longest separator title.
#' @param line_nr A boolean value that indicates whether the line numbers should
#'   be printed along with the structure summary.
#' @param granularity Indicates the lowest level of granularity that should be
#'   included in the summary.
#' @param last_sep A boolean value indicating whether or not the separating
#'   lines of the highest granularity should be printed.
#' @param title A boolean value indicating whether the reported summary should
#'   contain a title or not.
#' @param header A boolean value indicating whether a column header should
#'   indicate the name of the columns (line, level, section).
#' @param ... futher arguments to be passed from and to other methods, in
#'   particular \code{\link{list.files}} for reading in multiple files.
#' @details To create the summary, \code{sum_rdf} uses regular expressions.
#'   Hence it is crucial that the code separators and the separator titles
#'   match the regular expression pattern. We recommend inserting
#'   separators and their titles using the RStudio Add-in that is contained
#'   in this package. The definition is rather intuitive as can be seen in the
#'   example section below. However, we shall provide a formal definition here
#'   as well.
#'   \itemize{
#'     \item A code separator is defined as a line that starts with n hashes,
#'     followed by 4-n spaces where 0 < n < 4. This sequence is followed by one
#'     or more either \code{.} or \code{_}.
#'     \item A title associated with a code separator is defined as a line
#'     that starts with n hashes, followed by 4-n spaces where 0 < n < 4. This
#'     sequence is \emph{not} followed by \code{.} or \code{_}.
#'   }
#'   Lines that do not satisfy these requirements (e.g. do not start with #s,
#'   do not contain the right number of spaces after the #, indent before any #
#'   ect.) are not considered by \code{sum_rdf}.
#' @section Removing spaces and hashes:
#'   The add-in contained in this package inserts section titles in a way that
#'     that they are recognised by RStudio as sections (for details, see
#'     \href{https://support.rstudio.com/hc/en-us/articles/200484568-Code-Folding-and-Sections}{RStudio's official website}. One structure that is
#'     recognised by RStudio as section is a line starting with a hash and ending
#'     with four hashes. This structure is implemented with \code{strcode}.
#'     Hence when creating the summary, it might be desired to remove the right
#'     hand hashes and spaces, which can be specified with the respective options
#'     \code{rm_rh_hashes} and \code{rm_rh_spaces}.
#' @seealso insert_l_break
#' @examples
#' # the following separator is an example of a valid
#' # separator and associated title
#'
#' #   __________________________________________________
#' #   this is a level 1 title                     ####
#' ##  . . . . . . . . . . . . . . . . . . . . . . . . .
#' ##  note that the title or the separator character (_, .)
#' ##  always starts at indention 4.
#'
#' \dontrun{
#' # Open a new .R file in RStudio, insert some code breaks
#' # using the Add-in of this package, save the file and run:
#' sum_rdf() # get a summary of the source editor.
#' }
#' @importFrom rstudioapi getSourceEditorContext
#' @export
#'
#   ____________________________________________________________________________
#   user function                                                           ####
sum_rdf <- function(path_in = getSourceEditorContext()$path,
                    file_in_extension = ".R",
                    dir_out = "",
                    file_out = NULL,
                    file_out_extension = "",
                    width = NULL,
                    rm_rh_hashes = TRUE,
                    rm_rh_spaces = TRUE,
                    rm_break_anchors = TRUE,
                    line_nr = TRUE,
                    granularity = 3,
                    last_sep = FALSE,
                    title = TRUE,
                    header = TRUE,
                    ...) {

##  ............................................................................
##  assertive tests                                                         ####
assert_number(granularity, lower = 1, upper = 3)

### . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ..
### prepare input to call helper repeated times                             ####
  # check if file can be directory or path
  if (is.na(file.info(path_in)$isdir)) {
    stop("Indicated path (", path_in, ") is neither a directory nor a valid file name")
  }
  # create files if path_in is directory
  else if(file.info(path_in)$isdir) {
    all_files <- list.files(path = path_in,
                            pattern = paste0(file_in_extension, "$"),
                            full.names = FALSE)
    # files contain path name
    all_files <- paste(path_in, all_files, sep = "/")
    if (length(all_files) == 0) {
      warning("there are no files in the directory")
    }

  # in the case path_in is already a file
  } else {
    all_files <- path_in
  }

  # if output is not printed in the console, print a short summary.
### . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ..
### call helper                                                             ####
  if (dir_out != "") {
    cat("The following files were summarized \n")
  }
  output <- lapply(all_files, function(g) {
    # pass all arguments as is except the path_in
    if (dir_out != "") {
      cat(g, sep = " \n")
    }
    sum_rdf_helper(path_in = g,
                   dir_out = dir_out,
                   file_out = file_out,
                   file_out_extension = file_out_extension,
                   width = width,
                   rm_rh_hashes = rm_rh_hashes,
                   rm_rh_spaces = rm_rh_spaces,
                   rm_break_anchors = rm_break_anchors,
                   line_nr = line_nr,
                   granularity = granularity,
                   last_sep = last_sep,
                   title = title,
                   header = header)
  })

  if (dir_out == "" && !is.null(file_out) && file_out == "object") {
    output
  } else {
    invisible() # avoid unnecessary NULL return
  }
}
#' helper function for code summarisation
#   ____________________________________________________________________________
#   helper function: sum_rdf_helper                                         ####
#' return code summary for one file
#'
#' Function is called by \code{sum_rdf()} and returns summary of one code file.
#' @inheritParams sum_rdf
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
sum_rdf_helper <- function(path_in,
                           dir_out,
                           file_out,
                           file_out_extension,
                           rm_rh_hashes,
                           rm_rh_spaces,
                           rm_break_anchors,
                           width,
                           line_nr,
                           granularity,
                           last_sep,
                           title,
                           header) {

##  ............................................................................
##  argument interaction                                                    ####
### .. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
### get the file_out together
  if (is.null(file_out)) {
    # file_out must be a file name. Grap the last expression after backslash.
    backslash_rm <- gsub("^.*/(.*)", "\\1", path_in, perl = TRUE)
    file_out <- paste0("code_summary-",
                       gsub("^(.*)\\..*$", "\\1", backslash_rm, perl = TRUE),
                       file_out_extension)
  }

### .. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
### paths

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
  sub_pattern <- "^#   [^ ]|^##  [^ ]|^### [^ ]"
  cand <- grep(sub_pattern, lines, perl = FALSE) # extract candiates id
  if (length(cand) == 0) {
    return(warning("No line matching the required pattern",
                   call. = FALSE, immediate. = TRUE))
  }

##  .................. #< 3b5746a13447c5269736b631d6a9370d ># ..................
##  replace hashed seps                                                     ####
if (rm_break_anchors) {
  # extract candidates for replacement
  hash_candid <- intersect(grep("(\\s#<\\s[0-9a-z]{1,33}\\s>#\\s)", lines, perl = TRUE),
                           cand)
  # get their level
  lvl <- nchar(gsub("^(#+)\\s.*$", "\\2", lines[hash_candid], perl = TRUE))
  replacement <- vapply(lvl, function(x) help_create_break(start = paste0(rep("#", x), collapse = ""),
                                           break_char = give_breakchar(x),
                                           sep = paste(rep(" ", 4 - x), collapse = ""), anchor_in_sep = FALSE),
                  FUN.VALUE = character(1))
  lines[hash_candid] <- replacement

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
### remove last separator
  if (last_sep == FALSE) {
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

  # issue warning if there are no titles
  pattern_titles <- grep("^(#   |##  |### )[^\\._)].*$", lines, value = TRUE)
  if (length(pattern_titles) == 0) { # if there were no titles
    warning("There are no segment titles.",
            call. = FALSE, immediate. = TRUE)
  }

  # remove right hand hashes if desired
  if (rm_rh_hashes) {
    lines <- gsub("####$", "    ", lines, perl = TRUE)
  }

  # remove right hand spaces if desired
  if (rm_rh_spaces) {
    lines <- gsub("\\s*$", "", lines, perl = TRUE)
  }

  # adjust length of pattern.
  if (is.null(width)) {
  # first calculate width. It is the length of the maximal title string
  ## get the title strings
    if (length(pattern_titles) == 0) { # if there were no titles
      width <- options()$strcode$char_length
      warning("width set to options()$strcode.char.length",
              call. = FALSE, immediate. = TRUE)
    } else { # if there were titles
      width <- max(nchar(pattern_titles))
    }
  }

  lines <- substring(lines, 1, width)

  if (line_nr == TRUE) {
    lines <- paste(tokeep, lines, sep = "\t")
  }

  if (header == TRUE) {
    lines <- append(c("line  level section"), lines)
  }
  if (title == TRUE) {
    lines <- append(paste0("Summarized structure of ", path_in, "\n"), lines)
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
#' space sequence (see \code{\link{sum_rdf}}). The search is implemented using
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
