#' insert a section break with an optional title
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
#' @seealso \code{\link{sum_str}}
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
#' @param level The level, a numeric value bounded between 1 and 3
#' @param insert_with_shiny A boolean value indicating whether to use
#'   a shiny gadget to add separator and possibly title. If set to \code{FALSE},
#'   simply a separator will be inserted and the user has to set the title
#'   himself. To permanently set
#'   this argument, you can alter the global option strcode$insert_with_shiny,
#'   which is the location where \code{insert_break} looks up the value when
#'   used as an RStudio Add-in.
#' @keywords internal
insert_break <- function(level,
                         insert_with_shiny = options()$strcode$insert_with_shiny){

  ### .. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
  ### elicit title of section (and possible reset level)

  if (insert_with_shiny) {
    ret_value <- find_title(level)
    if (ret_value$cancel) return("")
##  ............................................................................

    if (ret_value$add_semantics) {
      # create fill
      creators <- setNames(Map(create_creators,
                             #start = c("", "#", ".", "")),
                             start = c("", "", "", "")),  
                         #c("get_title", "get_id", "get_class","get_attribute"))
                           c("get_id", "get_class", "get_title", "get_attribute"))
      tempstring=ret_value$id      
      
      fill <- create_fill(id = ret_value$id,
                          classes = ret_value$classes,
                          title = ret_value$text1,
                          attributes = ret_value$keyvaluepairs,
                    json_ld = ret_value$json_ld,
                    function_container = creators)
      #fill <- create_fill(id = paste0("\"@id\":",ret_value$id,"\,"),
      #                    classes = paste0("\"@type\":",ret_value$classes,"\,"),
      #                    title = paste0("\"label\":",ret_value$text1,"\,"),
      #                    attributes = ret_value$keyvaluepairs,
      #                    function_container = creators)

    } else {
      fill <- ret_value$text1
    }
    anchor_in_sep <- ret_value$anchor_in_sep

    # set options so anchor_in_sep is remembered
    op <- options()$strcode
    op$anchor_in_sep <- anchor_in_sep
    options(strcode = op)
    level <- as.numeric(unlist(strsplit(ret_value$level, ""))[nchar(ret_value$level)])


  } else {
    fill <- ""
    anchor_in_sep <- FALSE
  }
  ### .. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
  ### set parameter depending on level
  start <- paste0(rep("#", level), collapse = "")
  break_char = give_breakchar(level)
  sep = paste(rep(" ", 4 - level), collapse = "")
  #start <- paste0(rep("#", 3), collapse = "")
  #break_char = give_breakchar(3)
  #sep = paste(rep(" ", 4 - 3), collapse = "")



#   ____________________________________________________________________________

### .. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
### create break sequence to insert
  seq_break <- help_create_break(start = start,
                                 break_char = break_char,
                                 sep = sep, anchor_in_sep = anchor_in_sep)
  help_insert(seq_break,
              start_row = 1,
              start_indention = Inf,
              start_indention_margin = 0,
              end_row = 2,
              end_indention = Inf)

### .. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
### create fill sequence to insert
  if (insert_with_shiny) {
    seq_title <- help_create_title(start = start,
                                   fill = fill,
                                   sep = sep,
                                   end = "####",
                                   enforce_length = !ret_value$add_semantics)


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
#' @param start A sequence of letters to start the sequence.
#' @param sep A separator sequence to separate start and break_char.
#' @param break_char A character (sequence) used to create the actual break.
#' @param length An integer value indicating how long the sequence should be.
#' @param anchor_in_sep whether or not a code anchor (that is, a hash) should
#'   be inserted in the center of the separator.
#' @keywords internal
help_create_break <- function(start = "##",
                              sep = " ",
                              break_char = "-",
                              length = options()$strcode$char_length,
                              anchor_in_sep = FALSE) {
  #
  if (anchor_in_sep == TRUE) {
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
#' @keywords internal
help_create_title <- function(start = "##",
                              fill = "this is a title",
                              length = options()$strcode$char_length,
                              sep = "sep_here",
                              end = "----",
                              enforce_length = TRUE) {
  # create a text that starts with start, adds sep and then spaces up to margin
  # too long texts will be truncated
  if (fill == "") return(NULL)
  
  text <- paste0(start, sep, fill)
  #text <- paste0(start, sep, paste0("\"@id\":",ret_value$id),
  #               paste0("\"@type\":",ret_value$classes),
  #               paste0("\"label\":",ret_value$text1),ret_value$keyvaluepairs)
  
  

  extension <- paste0(rep(" ",
                          max(0, length - nchar(end) - nchar(text))),
                      collapse = "")
  str_length <- ifelse(enforce_length, length - nchar(end), nchar(extension) + nchar(text))

  paste0(substring(paste0(text, extension), 1, str_length), end)
}

#' find breakchar for level
#'
#' minimal helper to return breakf for a given level
#' @param level the level for which the break character should be returned
#' @keywords internal
give_breakchar <- function(level) {
  switch(as.character(level),
       "1" = "_",
       "2" = ".",
       "3" = ". ")
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
#' @keywords internal
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
#' @importFrom stats setNames
#' @keywords internal
find_title <- function(level) {
  #XJ Original codes:
  #styles_input <- paste(c("Default","JSON-LD"))
  choices_input <- paste("level", 1:3)
  #class_choices <- paste("class", 1:10)
  #choices_input <- paste(c("Workflow","Trace","Data Structure"))
  #class_choices <- paste(c("Process","InputPort","OutputPort","DataLink","SeqCtrlLink","Workflow","User","hasSubProcess","sourcePToCL",
  #                         "CLtoDestP","hasInPort","hasOutPort","hasDefaultParam","DLToInPort","outPortToDL","inPortToDL","DLToOutPort",
  #                         "wasAttributedTo","wasDerivedFrom","ProcessExec","dataOnLink","used","wasGeneratedBy","wasAssociatedWith",
  #                         "wasInformedBy","isPartOf","Data","Collection","wasDerivedFrom","hadMember"))
  class_choices <- paste(c("provone:Process","provone:InputPort","provone:OutputPort","provone:DataLink","provone:SeqCtrlLink",
                           "provone:Workflow","provone:User","provone:ProcessExec","provone:Data","provone:Collection","provone:Visualization"))
  ui <- miniPage(
    miniContentPanel(
      fillCol(
        fillRow(
          text_focus("text1", label = " ", value = "",
                     placeholder = "Your section title",
                     width = "320px", height = "35px"),
          selectInput("level", " ", width = "100px",
                      choices = choices_input,
                      selected = choices_input[level]),
          flex = c(3, 1)
        ),
        fillRow(
          miniTitleBarCancelButton(),
          miniTitleBarButton("done", "Done"),
          miniTitleBarButton("show", "Help"),
          checkboxInput("anchor_in_sep", "Add anchor",
                        value = options()$strcode$anchor_in_sep,
                        width = "100px"),
          checkboxInput("add_semantics", "Add semantics",
                        value = FALSE,
                        width = "150px"),
          flex = c(1, 1, 1, 1.5, 2)
        ),
        fillRow(
          conditionalPanel("input.add_semantics",
                           text_focus("pandoc_id", label = "identifier", value = get_anchor("", "", nchar_output = 5),
                                      placeholder = "enter a unique identifier",
                                      width = "320px", height = "35px"),
                           selectizeInput("classes", label = "classes",
                           choices = setNames(rm_space(class_choices), class_choices),
                           width = "320px",
                          multiple = TRUE),
                           selectizeInput("keyvaluepairs", width = "320px",
                                          label = "Manually input: property=value",
                           choices = "", multiple = TRUE,
                           options = list(create = TRUE,
                                          persist = FALSE,
                                          createFilter = "^[a-zA-Z0-9:]+\\s*=\\s*[a-zA-Z0-9]+$")),
                           checkboxInput("json_ld", "JSON-LD"，width = "80px")
                           )
        )
        ,flex = c(0.8, 0.5, 3)
      )
      #,
      #fillRow(
      #  selectInput("style", " ", width = "200px",
      #                choices = styles_input,
      #                selected = styles_input[level])
      #checkboxInput("jsonld_style", "JSON-LD style",
      #                  value = FALSE,
      #                  width = "150px"),
      #checkboxInput("default_style", "Default style",
      #                  value = TRUE,
      #                  width = "150px"),
      #flex = c(1, 1.5)
      #),
    )
  )

  server <- function(input, output, session) {
    listout <- quote(list(text1  = gsub("\n", "", input$text1),
                    cancel = input$cancel,
                    anchor_in_sep = input$anchor_in_sep,
                    add_semantics = input$add_semantics,
                    id = input$pandoc_id,
                    level  = input$level,
                    classes = input$classes,
                    keyvaluepairs = input$keyvaluepairs,
                          json_ld = input$json_ld
                    #style=input$style
                    #,jsonld_style=input$jsonld_style,
                    #default_style=input$default_style    
                         ))

    observeEvent(input$done, {
      stopApp(eval(listout))
    })

    observeEvent(input$show, {
      showModal(modalDialog(
        title = "Help",
        "Hit enter (instead of clicking Done) to confirm the title.
        An empty field will create a separator with no title.
        If the field of the section identifier remains empty, a
        hash is generated as an identifier. Valid key value pairs
        take the form key = value where key and value can only contain
        numbers and letters"
      ))
    })

    observeEvent(input$text1, {
      if(!is.null(input$text1) && any(grep("\n", input$text1))) {
        stopApp(eval(listout))
      }
    })

    observeEvent(input$cancel, {
      stopApp(eval(listout))
    })
  }

  runGadget(ui, server,
            viewer = paneViewer(minHeight = 400),
            stopOnCancel = FALSE)
}

