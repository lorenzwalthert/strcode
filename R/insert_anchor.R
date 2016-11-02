#' Insert a hash anchor
#'
#' @param length_random_input A number giving the range from which to withdraw
#'   a random number.
#' @param enclosing_start A string that specifies the right-hand side enclosing
#'   of the hash
#' @param enclosing_end A string specifies the left-hand-side of the enclosing
#' @param ... further arguments to be passed to and from other methods, in
#'   in particular to digest
#' @details The function draws a random number using sample from the range of
#'   \code{length_random_input}. Then, this number is hashed calling the
#'   \code{digest} hash-function from the \code{digest} package. This hash
#'   is then wrapped into \code{enclosing}, whereas the order of string
#'   specified with \code{enclosing} is reversed on the right side of the hash if
#'   enclosing_end is \code{NULL}.
#' @examples \dontrun{
#'   # RStudio needs to run for this
#'   insert_anchor(enclosing = "#!")
#'   }
#' @importFrom digest digest
#' @export


##  ............................................................................
insert_anchor <- function(length_random_input = .Machine$integer.max,
                          enclosing_start = "#<",
                          enclosing_end = ">#",
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


##  ............................................................................
##  actual function call

  # a function to insert a random hash
  random_nr <- sample(length_random_input, 1)
  hash <- digest(random_nr)

  # hash should be inserted at active row, then, cursor should jump to next row
  anchor <- paste(enclosing_start, hash, enclosing_end) # wrap hash in a
  help_insert(anchor,
              start_row = 0,
              start_indention = Inf,
              start_indention_margin = 1,
              end_row = 1)
}
