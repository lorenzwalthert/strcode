#' insert a section break with an optional title


#   ____________________________________________________________________________
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
#'     #   . . . . . . . . . . . . . . . . . . . . . . . ..
#' }
#' For optimal use, we recommend specifying keyboard shortcuts in the add-in
#'   settings. A title can be added to the section as well. When calling the
#'   function, a shiny app is
#'   opened in the viewer pane, where the title can be specified. If the field
#'   remains empty, only a section break is created without title. You may hit
#'   enter instead of clicking on "done" to confirm your choice.
#'
#' @details The breaks characters (\code{___}, \code{...}, \code{. .}) were
#'  chosen such that they reflect the level of granularity, namely \code{___}
#'  has a much higher visual density than \code{. .} \cr
#'  We recommend starting off by grouping code into level 2 blocks.
#'  The advantage is that in both directions of granularity, there is another
#'  layer (\code{___} and \code{...}) left. When the code base grows, there
#'  might be a need to extend in both directions. \cr
#'  In order to be \href{https://support.rstudio.com/hc/en-us/articles/200484568-Code-Folding-and-Sections}{recognised as sections by RStudio},
#'  all titles end with four hashes (see example below). We put the hashes at
#'  the end of the line (where end of a line is defined by the global option
#'  options()$strcode$char_length, which defaults to 80)
#'  and separate it from the section title with spaces to
#'  achieve a natural representation in the code flow.
#'  Being recognised as sections by RStudio means
#'  that at the very botton of the code pane, right next to line/indent count,
#'  is a little table of contents of the current file which can be expanded to
#'  view (and jump to) different code sections. Additionally, you will be able
#'  to fold code sections (just as you can fold function declarations in RStudio).
#'
#' @name insert_l_break
#' @seealso sum_str
#' @importFrom rstudioapi insertText getActiveDocumentContext setCursorPosition
#' @examples
#' # This is a minimal example.
#' # See the readme for a longer and more detailed example.
#'
#' ##  ......................................................
#' ##  A: pre-process t2                                 ####
#' ### .. . . . . . . . . . . . . . . . . . . . . . . . . . .
#' ### a: substep 1                                      ####
#'
#'
#'
#' # [your code here]
#'
#'
#'
#' ### .. . . . . . . . . . . . . . . . . . . . . . . . . . .
#' ### b: substep 2                                      ####
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
#' @rdname insert_l_break
#' @aliases insert_l1_break
#' @export
insert_l1_break <- function() {
  insert_break(level = 1)
}

##  ............................................................................
##  level 2
#' @rdname insert_l_break
#' @aliases insert_l2_break
#' @export
insert_l2_break <- function() {
  insert_break(level = 2)
}

##  ............................................................................
##  level 3
#' @rdname insert_l_break
#' @aliases insert_l1_break
#' @export
insert_l3_break <- function() {
  insert_break(level = 3)
}

#   ____________________________________________________________________________
#   helper functions

##  ............................................................................
##  top level

#' Insert a code break of arbitrary level
#'
#'A helper function to insert a code break for a given level
#' @param level The level, a numeric value bounded by 1 and 3
#' @param insert_with_shiny A boolean value indicating whether to use
#'   a shiny gadget to add separator and possibly title. If set to \code{FALSE},
#'   simply a separator will be inserted and the user has to set the title
#'   himself. To permanently set
#'   this argument, you can alter the global option strcode$insert_with_shiny,
#'   which is the location where \code{insert_break} looks up the value when
#'   used as an RStudio Add-in.
insert_break <- function(level,
                         insert_with_shiny = options()$strcode$insert_with_shiny){

  ### .. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
  ### elicit title of section (and possible reset level)

  if (insert_with_shiny) {
    ret_value <- find_title(level)
    if (ret_value$cancel) return("")
    title <- ret_value$text1
    hash_in_sep <- ret_value$hash_in_sep

    # set options so hash_in_sep is remembered
    op <- options()$strcode
    op$hash_in_sep <- hash_in_sep
    options(strcode = op)
    level <- as.numeric(unlist(strsplit(ret_value$level, ""))[nchar(ret_value$level)])


  }
  ### .. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
  ### set parameter depending on level
  start <- paste0(rep("#", level), collapse = "")
  break_char = switch(as.character(level),
                      "1" = "_",
                      "2" = ".",
                      "3" = ". ")
  sep = paste(rep(" ", 4 - level), collapse = "")



  #   ____________________________________________________________________________


  ### .. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
  ### create break sequence to insert
  seq_break <- help_create_break(start = start,
                                 break_char = break_char,
                                 sep = sep, hash_in_sep = hash_in_sep)
  help_insert(seq_break,
              start_row = 1,
              start_indention = Inf,
              start_indention_margin = 0,
              end_row = 2,
              end_indention = Inf)

### .. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
### create title sequence to insert
  if (insert_with_shiny) {
    seq_title <- help_create_title(start = start,
                                   fill = title,
                                   sep = sep,
                                   end = "####")


    if (!is.null(seq_title)) {
      help_insert(seq_title,
                  start_row = 0,
                  start_indention = Inf,
                  start_indention_margin = 0,
                  end_row = 1,
                  end_indention = Inf)
    }
  }
}
##  ............................................................................
##  help_create_break
#' create a break sequence
#'
#' the idea of the helper function is to return a string of a line length that is
#' composed of the start character and the break_characters
#' @param start A sequence of letters to start the sequence
#' @param sep A separator sequence to separate start and break_char
#' @param break_char A character (sequence) used to create the actual break
#' @param length An integer value indicating how long the sequence should be
#' @param hash_in_sep whether or not a hash should be inserted in the center of
#'   the separator
help_create_break <- function(start = "##",
                              sep = " ",
                              break_char = "-",
                              length = options()$strcode$char_length,
                              hash_in_sep = FALSE) {
  #
  if (hash_in_sep == TRUE) {
    hash <- get_anchor(enclosing_start = "#<",
                       enclosing_end = ">#",
                       length_random_input = .Machine$integer.max)
    # recalculate length
    hash_length <- nchar(hash) - 2
    current_length <- (length - hash_length) / 2

  } else {
    current_length <- length
    hash <- NULL
  }

  breaks <- rep(break_char,
                # ceiling necessary because patern like ". ." will get cut before
                # current_length
                ceiling((current_length - nchar(start) - nchar(sep))/nchar(break_char)))
  # if last element in breaks is space, replace it with first element in break_char
  breaks <- unlist(strsplit(breaks, "")) # decompose
  if (breaks[length(breaks)] == " ") {
    breaks[length(breaks)] <- substring(break_char, 1, 1)
  }
  breaks <- paste0(breaks, collapse = "")
  # paste it all together
  temp <- paste0(c(start, sep, breaks, " ", hash, " ", breaks), collapse = "")
  substring(temp, 1, length) # truncate pattern to exacly current_length
}
##  ............................................................................
##  help_create_title
#' create a title sequence
#'
#' This function returns a string that can be used as a title.
#' @param start A sequence of letters to start the sequence
#' @param fill A sequence to fill the sequence with. This is the actual title.
#' @param length An integer value indicating how long the sequence should be
#' @param sep A separator sequence to separate start and fill.
#' @param end A character sequence that indicates what the end of the final
#'   sequence should look like.
#' @details
#'   For Rstudio to recognize a hereby produced sequence as a title, it must
#'     start with # and end with at least 4 of the following characters: #, -, =.
help_create_title <- function(start = "##",
                              fill = "this is a title",
                              length = options()$strcode$char_length,
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
##  help insert                                                             ----
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
#' @param level The level of the code break to be inserted
#' @import shiny miniUI
find_title <- function(level) {
  choices_input <- paste("level", 1:3)
  ui <- miniPage(
    miniContentPanel(
      fillCol(
        fillRow(
          text_focus("text1", label = " ", value = "",
                     placeholder = "Your section title",
                     width = "320px", height = "33px"),
          selectInput("level", " ", width = "100px",
                      choices = choices_input,
                      selected = choices_input[level]),
          flex = c(3, 1)
        ),
        fillRow(
          miniTitleBarCancelButton(),
          miniTitleBarButton("done", "Done"),
          checkboxInput("hash_in_sep", "Add hash",
                        value = options()$strcode$hash_in_sep,
                        width = "100px"),
          p("Hit enter (instead of clicking ok) to confirm the title. An empty
            field will create a separator with no title."),
          flex = c(1, 1, 2, 3)

          )
      )
    )
    )

  server <- function(input, output, session) {

    observeEvent(input$done, {
      stopApp(list(text1  = input$text1,
                   cancel = input$cancel,
                   hash_in_sep = input$hash_in_sep,
                   level  = input$level))
    })

    observeEvent(input$text1, {
      if(!is.null(input$text1) && any(grep("\n", input$text1))) {
        stopApp(list(text1 = gsub("\n", "", input$text1),
                     cancel = input$cancel,
                     hash_in_sep = input$hash_in_sep,
                     level  = input$level))
      }
    })

    observeEvent(input$cancel, {
      stopApp(list(text1 = "",
                   cancel = input$cancel))
    })
  }

  runGadget(ui, server,
            viewer = paneViewer(minHeight = 200),
            stopOnCancel = FALSE)
}

