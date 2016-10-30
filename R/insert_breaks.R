#' insert segment, section or subsection break
#'
#' A function designed to use as an RStudio
#'   \href{https://rstudio.github.io/rstudioaddins/}{add-in} for structuring code.
#'   There are three levels granularity
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
#' @importFrom rstudioapi insertText
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
  to_insert <- help_insert(start = "#",
                           break_char = "_",
                           sep = "   ")
  insertText(to_insert)
}

##  ............................................................................
##  level 2
#' @rdname insert_break
#' @export
insert_l2_break <- function() {
  to_insert <- help_insert(start = "##",
                           break_char = ".",
                           sep = "  ")
  insertText(to_insert)
}

##  ............................................................................
## level 3
#' @rdname insert_break
#' @export
insert_l3_break <- function() {
  to_insert <- help_insert(start = "###",
                           break_char = ". ",
                           sep = " .")
  insertText(to_insert)
}



#   ____________________________________________________________________________
#   helper functions: help_insert
# the idea of the helper function is to return a string of a line length that is
# composed of the start character and the break_characters
help_insert <- function(start = "##",
                        break_char = "-",
                        lenght = options()$strcode.char.lenght,
                        sep = " ") {
  paste(paste(start, "", sep = sep),
        paste(rep(
            break_char,
            ceiling((lenght - nchar(start) - nchar(sep))/nchar(break_char))),
          collapse = ""),
        "\n",
        # start, paste(rep(" ", 4 - nchar(start)), collapse = ""),
        # insert new line and corresponding comment structure
        sep = "")
}
