#' insert segment, section or subsection break
#'
#' A function designed to use as an RStudio
#'   \href{https://rstudio.github.io/rstudioaddins/}{add-in} for structuring code.
#'   There are three levels granularity
#'   \itemize{
#'     \item segments (level III), which are high-level blocks that can be separated
#'       as follows \cr    ### ======================
#'     \item sections (level II), which are medium-level blocks that can be separated
#'       as follows \cr    ## -----------------------------------------
#'     \item subsections (level I), which are low-level blocks that can be separated
#'       as follows \cr # ...................................................}
#'   For optimal use, we recommend specifying shortcuts in the add-in settings.
#' @details The breaks characters (\code{=}, \code{-}, \code{.}) were choosen
#'   such that they reflect the level of granularity, namely \code{===} has a
#'   much higher visual density than
#'   \code{...} \cr
#'   We recommend to start off grouping code into level II blocks.
#'   The advantage is that in both directions of granularity,
#'   there is another layer (\code{===} and \code{...}) left.
#'   When the code base grows, there
#'   might be the need to extend in both directions.
#' @name insert_break
#' @importFrom rstudioapi insertText
#' @examples
#'   ## ===============================================
#'   ## Section 1: load data
#'   # ................................................
#'   # Subsection a: load first data set
#'
#'   # [your code here]
#'
#'   # ................................................
#'   # Subsection b: load second data set
#'
#'   # [your code here]
#'
#'   ## ==============================================

NULL

#' @rdname insert_break
#' @export
insert_l1_break <- function() {
  to_insert <- help_insert(start = "#",
                           break_char = "_",
                           sep = "   ")
  insertText(to_insert)
}

#' @rdname insert_break
#' @export
insert_l2_break <- function() {
  to_insert <- help_insert(start = "##",
                           break_char = ".",
                           sep = "  ")
  insertText(to_insert)
}

#' @rdname insert_break
#' @export
insert_l3_break <- function() {
  to_insert <- help_insert(start = "###",
                           break_char = ". ",
                           sep = " .")
  insertText(to_insert)
}




help_insert <- function(start = "##",
                        break_char = "-",
                        lenght = 80,
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
