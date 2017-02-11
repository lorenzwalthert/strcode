
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
