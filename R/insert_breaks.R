#' insert segment, section or subsection break
#'
#' A function designed to use as an RStudio
#'   \href{https://rstudio.github.io/rstudioaddins/}{add-in} for structuring code.
#'   There are three levels of granularity
#'   \itemize{
#'     \item level 1 sections, which are high-level blocks that can be separated
#'       as follows \cr    ### ______________________
#'     \item level 2 sections, which are medium-level blocks that can be separated
#'       as follows \cr    ##  ..............................................
#'     \item level 1 sections, which are low-level blocks that can be separated
#'       as follows \cr #   .. . . . . . . . . . . . . . . . . . . . . . . .}
#'   For optimal use, we recommend specifying shortcuts in the add-in settings.
#' @details The breaks characters (\code{___}, \code{...}, \code{. .}) were chosen
#'   such that they reflect the level of granularity, namely \code{___} has a
#'   much higher visual density than
#'   \code{. .} \cr
#'   We recommend to start off grouping code into level 2 blocks.
#'   The advantage is that in both directions of granularity,
#'   there is another layer (\code{===} and \code{...}) left.
#'   When the code base grows, there
#'   might be the need to extend in both directions.
#' @name insert_break
#' @importFrom rstudioapi insertText getActiveDocumentContext setCursorPosition
#' @examples
#' # this is a minimal example.
#' # See the readme for a longer and more detailed example.
#'
#' ##  ......................................................
#' ##  A: pre-process t2
#' ### .. . . . . . . . . . . . . . . . . . . . . . . . . . .
#' ### a: substep 1
#'
#'
#'
#' # [your code here]
#'
#'
#'
#' ### .. . . . . . . . . . . . . . . . . . . . . . . . . . .
#' ### b: substep 2
#'
#'
#'
#' # [your code here ]
#'
#'
#'
#' ##  ......................................................
NULL


#   ____________________________________________________________________________
#   exported functions
##  ............................................................................
#' @rdname insert_break
#' @export
insert_l1_break <- function() {
  to_insert <- help_create_break(start = "#",
                           break_char = "_",
                           sep = "   ")
  help_insert(to_insert, start = 7, end = 2)
}

##  ............................................................................
##  level 2
#' @rdname insert_break
#' @export
insert_l2_break <- function() {
  to_insert <- help_create_break(start = "##",
                           break_char = ".",
                           sep = "  ")
  help_insert(to_insert, start = 1, end = 2)
}

##  ............................................................................
##  level 3
#' @rdname insert_break
#' @export
insert_l3_break <- function() {
  to_insert <- help_create_break(start = "###",
                           break_char = ". ",
                           sep = " .")
  help_insert(to_insert, start = 1, end = 2)
}



#   ____________________________________________________________________________
#   helper functions
##  ............................................................................
##  help_create_break
# the idea of the helper function is to return a string of a line length that is
# composed of the start character and the break_characters
help_create_break <- function(start = "##",
                        break_char = "-",
                        length = options()$strcode.char.length,
                        sep = " ") {

  temp <-
    paste(start, sep,
        paste(rep(break_char,
            # ceiling necessary because patern like ". ." will get cut before
            # length
            ceiling((length - nchar(start) - nchar(sep))/nchar(break_char))),
          collapse = ""),
        sep = "")
  substring(temp, 1, length) # truncate pattern to exacly length
}
##  ............................................................................
##  help_insert
# this funciton first gets the row in the active document, inserts a text x
# one row below and jumps another row down

help_insert <- function(x, start = 1, end = 2) {
  # get the row where the cursor is
  current_row <- getActiveDocumentContext()$selection[[1]]$range$start[1]
  # set the cursor to the very left of that row
  setCursorPosition(c(current_row, Inf))
  # insert end line breaks
  insertText(paste(rep("\n", start), collapse = ""))

  # insert the separator at the beginning of the new line, so \n gets
  # shifted down one
  insertText(c(current_row  + start, 1), x)
  # move the cursor one line down
  setCursorPosition(c(current_row + end, 1), id = NULL)

}

