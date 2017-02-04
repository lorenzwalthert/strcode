#' insert segment, section or subsection break
##  ............................................................................

#'
#' A function designed to use as an RStudio
#' \href{https://rstudio.github.io/rstudioaddins/}{add-in} for structuring code.
#'  \cr
#' There are three levels of granularity:
#' \itemize{
#'   \item level 1 sections, which are high-level blocks denoted by \cr
#'     ### ______________________
#'   \item level 2 sections, which are medium-level blocks denoted by \cr
#'     ##  ..............................................
#'   \item level 3 sections, which are low-level blocks denoted by \cr
#'     #   .. . . . . . . . . . . . . . . . . . . . . . . .
#' }
#' For optimal use, we recommend specifying keyboard shortcuts in the add-in
#' settings.
#' @details The breaks characters (\code{___}, \code{...}, \code{. .}) were
#'  chosen such that they reflect the level of granularity, namely \code{___}
#'  has a much higher visual density than \code{. .} \cr
#'  We recommend starting off by grouping code into level 2 blocks.
#'  The advantage is that in both directions of granularity, there is another
#'  layer (\code{___} and \code{...}) left. When the code base grows, there
#'  might be a need to extend in both directions.
#' @name insert_break
#' @importFrom rstudioapi insertText getActiveDocumentContext setCursorPosition
#' @examples
#' # This is a minimal example.
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
##  level 1
#' @rdname insert_break
#' @export
insert_l1_break <- function() {
  insert_break(granularity = 1)
}

##  ............................................................................
##  level 2
#' @rdname insert_break
#' @export
insert_l2_break <- function() {
  insert_break(granularity = 2)
}

##  ............................................................................
##  level 3
#' @rdname insert_break
#' @export
insert_l3_break <- function() {
  insert_break(granularity = 3)
}

#   ____________________________________________________________________________
#   helper functions

##  ............................................................................
##  top level
#' Insert a code break of arbitrary granularity
#' @param granularity The granularity, a numeric value bounded by 1 and 3
insert_break <- function(granularity){

### .. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
### set parameter depending on granularity
  start <- paste0(rep("#", granularity), collapse = "")
  break_char = switch(as.character(granularity),
                      "1" = "_",
                      "2" = ".",
                      "3" = ". ")
  sep = switch(as.character(granularity),
               "1" = "   ",
               "2" = "  ",
               "3" = " ")

### .. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
### elicit title of section
title <- find_title()

### .. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
### create title sequence to insert
seq_title <- help_create_title(start = start,
                               fill = title,
                               sep = sep)

### .. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
### create break sequence to insert
seq_break <- help_create_break(start = start,
                               break_char = break_char,
                               sep = sep)

### .. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
### actual insertion
  help_insert(seq_break,
              start_row = 1,
              start_indention = Inf,
              start_indention_margin = 0,
              end_row = 2,
              end_indention = Inf)
  if (!is.null(seq_title)) {
    help_insert(seq_title,
                start_row = 0,
                start_indention = Inf,
                start_indention_margin = 0,
                end_row = 1,
                end_indention = Inf)
  }
}

##  ............................................................................
##  help_create_break
# the idea of the helper function is to return a string of a line length that is
# composed of the start character and the break_characters
help_create_break <- function(start = "##",
                        break_char = "-",
                        length = options()$strcode.char.length,
                        sep = " ") {

  breaks <- rep(break_char,
      # ceiling necessary because patern like ". ." will get cut before
      # length
      ceiling((length - nchar(start) - nchar(sep))/nchar(break_char)))
  # if last element in breaks is space, replace it with first element in break_char
  breaks <- unlist(strsplit(breaks, "")) # decompose
  if (breaks[length(breaks)] == " ") {
    breaks[length(breaks)] <- substring(break_char, 1, 1)
  }
  breaks <- paste0(breaks, collapse = "")
  temp <-
    paste(start, sep,
        paste(breaks,
          collapse = ""),
        sep = "")
  substring(temp, 1, length) # truncate pattern to exacly length
}
##  ............................................................................
##  help_create_title
# the idea of the helper function is to return a string of a line length that is
# composed of the start character and the break_characters
help_create_title <- function(start = "##",
                              fill = "this is a title",
                              length = options()$strcode.char.length,
                              sep = "sep_here",
                              end = "----") {
  # create a text that starts with start, adds sep and then spaces up to margin
  # too long texts will be truncated
  if (fill == "") return(NULL)
  text <- paste0(start, sep, fill)

  extension <- paste0(rep(" ",
                max(0, length - length(start) - length(end) - length(sep))),
                collapse = "")


  paste0(substring(paste0(text, extension), 1, length - nchar(end)), end)
}

##  ............................................................................
##  help_insert
# this funciton first gets the row in the active document, inserts a text x
# one row below and jumps another row down
#' help insert
#'
#' A helper function to insert text
#' @param x An object to insert
#' @param start_row the start row of the insertion
#' @param start_indention The start position within the row
#' @param start_indention_margin A margin (i.e. spaces) that will be added
#'   at the target row before \code{x} is inserted.
#' @param end_row the row where the cursor should be after the insertion
#' @param end_indention The end position within the row
help_insert <- function(x,
                        start_row = 1,
                        start_indention = Inf,
                        start_indention_margin = 0,
                        end_row = 2,
                        end_indention = Inf) {
  # get the row where the cursor is
  current_row <- getActiveDocumentContext()$selection[[1]]$range$start[1]
  # set the cursor to the very left of that row
  setCursorPosition(c(current_row, Inf))

  # insert end_row line breaks
  insertText(paste(rep("\n", end_row), collapse = ""))

  # insert the margin at the target row
  insertText(c(current_row  + start_row, start_indention),
             paste(rep(" ", start_indention_margin), collapse = ""))


  # insert the separator at the beginning of the new line, so \n gets
  # shifted down one
  insertText(c(current_row  + start_row, start_indention), x)
  # move the cursor one line down
  setCursorPosition(c(current_row + end_row, end_indention), id = NULL)

}

#   ____________________________________________________________________________
#   shiny helper                                                            ----

#' elicit break titles via shiny gadget
#'
#' A helper function to create a pane to enter a title name
#' @import shiny miniUI
find_title <- function() {

  ui <- miniPage(
    miniContentPanel(
      textAreaInput("text1", " ", width = "300px", height = "30px"),
      miniTitleBarCancelButton(),
      miniTitleBarButton("done", "Done")
    )
  )

  server <- function(input, output, session) {

    observeEvent(input$done, {
      stopApp(input$text1)
    })

    observeEvent(input$text1, {
      if(!is.null(input$text1) && any(grep("\n", input$text1))) {
        stopApp(gsub("\n", "", input$text1))
      }
    })

    observeEvent(input$cancel, {
      stopApp("")
    })
  }

  runGadget(ui, server,
            viewer = dialogViewer("Insert Code Segment"),
            stopOnCancel = FALSE)
}
