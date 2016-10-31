#   ____________________________________________________________________________
#   function test
test <- function(x) {
##  ............................................................................
##  A: pre-processing
### .. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
### a: assertive tests
  # x
  if(missing(x) || is.null(x)){
    x <- character()
  }
  assert(
    # use check within assert
    check_character(x),
    check_factor(x),
    check_numeric(x)
  )

  # levels
  if(!missing(levels)){
    assert(
      check_character(levels),
      check_integer(levels),
      check_numeric(levels))
    levels <- na.omit(levels)

  }

  # labels
  if(!missing(labels)){
    assert(
      check_character(labels),
      check_numeric(labels),
      check_factor(labels)
    )
  }

### .. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
### b: coercion / remove missing
  x <- as.character(x)
  uniq_x <- unique(na.omit(x), nmax = nmax)

### .. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
### c: warnings

  if(length(breaks) == 1) {
    if(breaks > max(x) - min(x) + 1) {
      stop("range too small for the number of breaks specified")
    }
    if(length(x) <= breaks) {
      warning("breaks is a scalar not smaller than the length of x")
    }
  }

##  ............................................................................
##  B: actual function
  variable < -paste("T", period, "nog_", sector, sep = "")
  variable <- paste(variable, "==", 1, sep = "")

  arg<-substitute(variable)
  r<-eval(arg, idlist.data[[1]])
  a<<-1

  was_factor <- FALSE
  if (is.factor(yes)) {
    yes <- as.character(yes)
    was_factor <- TRUE
  }
  if (is.factor(no)) {
    no <- as.character(no)
    was_factor <- TRUE
  }
  out <- ifelse(test, yes, no)
  if(was_factor) {
    cfactor(out)
  } else {
    out
  }

##  ............................................................................
}
#   ____________________________________________________________________________
#   function test2
test2 <- function(x) {
##  ............................................................................
##  A: pre-processing
### .. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
### a: assertive tests
  # x
  if(missing(x) || is.null(x)){
    x <- character()
  }
  assert(
    # use check within assert
    check_character(x),
    check_factor(x),
    check_numeric(x)
  )

  # levels
  if(!missing(levels)){
    assert(
      check_character(levels),
      check_integer(levels),
      check_numeric(levels))
    levels <- na.omit(levels)

  }

  # labels
  if(!missing(labels)){
    assert(
      check_character(labels),
      check_numeric(labels),
      check_factor(labels)
    )
  }

### .. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
### b: coercion / remove missing
  x <- as.character(x)
  uniq_x <- unique(na.omit(x), nmax = nmax)

### .. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
### c: warnings

  if(length(breaks) == 1) {
    if(breaks > max(x) - min(x) + 1) {
      stop("range too small for the number of breaks specified")
    }
    if(length(x) <= breaks) {
      warning("breaks is a scalar not smaller than the length of x")
    }
  }

##  ............................................................................
##  B: actual function
  variable < -paste("T", period, "nog_", sector, sep = "")
  variable <- paste(variable, "==", 1, sep = "")

  arg<-substitute(variable)
  r<-eval(arg, idlist.data[[1]])
  a<<-1

  was_factor <- FALSE
  if (is.factor(yes)) {
    yes <- as.character(yes)
    was_factor <- TRUE
  }
  if (is.factor(no)) {
    no <- as.character(no)
    was_factor <- TRUE
  }
  out <- ifelse(test, yes, no)
  if(was_factor) {
    cfactor(out)
  } else {
    out
  }

##  ............................................................................
}
