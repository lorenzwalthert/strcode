get_str <- function(dir = "./vignettes/",
                    file = "example.R",
                    width = 50,
                    line_nr = TRUE,
                    separator = TRUE) {

  path <- paste0(dir, file)
  lines <- readLines(con = path)
  pos <- grep("^#+([[:space:]]){1,4}", lines) # extract candiates
  # allow spaces in the beginning (deactivated)
  # pos <- grep("^[[:space:]]*#+([[:space:]]){1,4}", lines)
  pattern <-lines[pos]


  if (separator == FALSE) {
    remove <- grep("^#+\\s+[_|\\.|\\..\\s]+$", lines[pos], perl = TRUE)
    pattern <- pattern[-remove]
  }


  if (!is.null(width)) {
    pattern <- substring(pattern, 1, width)
  }

  if (line_nr == TRUE) {
    pattern <- paste(pos, pattern, sep = "\t")
  }





  cat(pattern, sep = "\n")
}

# relace ^\s+(#+) with \1 in Rstudio to move all breaks to the left.

