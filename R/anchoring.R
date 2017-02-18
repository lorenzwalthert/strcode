#' Insert a hash anchor
#' @inherit get_anchor
#' @details The function draws a random number using sample from the range of
#'   \code{length_random_input}. Then, this number is hashed by calling the
#'   \code{digest} hash-function from the \code{digest} package. This hash
#'   is then wrapped between \code{enclosing_start} and \code{enclosing_end}.
#' @examples \dontrun{
#'   # RStudio needs to run for this
#'   insert_anchor(enclosing_start = "#!")
#'   }
#' @importFrom digest digest
#' @export


##  ............................................................................
insert_anchor <- function(enclosing_start = "#<",
                          enclosing_end = ">#",
                          length_random_input = .Machine$integer.max,
                          ...) {

  # create anchor
  anchor <- get_anchor(enclosing_start = enclosing_start,
                       enclosing_end = enclosing_end,
                       length_random_input = length_random_input,
                       ...)

##  ............................................................................
##  set margin for insertion                                                ####
  # if you are at beginning of line: no margin
  indent <- getActiveDocumentContext()$selection[[1]]$range$start[2]
  margin <- ifelse(indent == 1, 0, 1)

##  ............................................................................
##  actual function call
  help_insert(anchor,
              start_row = 0,
              start_indention = Inf,
              start_indention_margin = margin,
              end_row = 1)
}

#   ____________________________________________________________________________
#   get_anchor                                                              ####
#' Creat anchor sequence
#'
#' Helper function to create an anchor string
#' @param length_random_input A number giving the range from which to draw
#'   a random number.
#' @param enclosing_start A string that specifies the left-hand side enclosing
#'   of the hash
#' @param enclosing_end A string that specifies the right-hand-side enclosing of
#'  the hash. If \code{NULL}, then the reverse of \code{enclosing_start} is used.
#' @param ... further arguments to be passed to and from other methods, in
#'   in particular to digest
#' @keywords internal
get_anchor <- function(enclosing_start = "#<",
                       enclosing_end = ">#",
                       length_random_input = .Machine$integer.max,
                       ...) {
##  ............................................................................
##  assertive test
  if(!grepl("^#", enclosing_start)) {
    stop(paste("enclosing_start should start with #, otherwise, ",
               "it might not be interpreted as a comment"))
  }

##  ............................................................................
##  argument interaction
  # enclosing
  if (is.null(enclosing_end)) {
    # create enclosing
    enclosing_end <- paste0(rev(unlist(strsplit(enclosing_start, ""))), collapse = "")

  }


  # a function to insert a random hash
  random_nr <- sample(length_random_input, 1)
  hash <- digest(random_nr)

  # return anchor
  paste(enclosing_start, hash, enclosing_end)

}
