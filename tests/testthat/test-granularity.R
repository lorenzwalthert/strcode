# This file checks all combinations of existing granularity in a document and
# requested granularity of the summary for consistency.



#   ____________________________________________________________________________
#   check 3 granularities last_sep = TRUE
context("granularity example-1")
path_start <- NULL
#path_start <- "tests/testthat/"

test_that("graunlarity example-1", {
### .. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
### check for granularity == 3

# sum_str(file_in = "example-1.R", dir_in = "tests/testthat/raw_in", granularity = 3,
# dir_out = "tests/testthat/correct", file_out = "out-ex-1_gran-3", title = FALSE)
  correct <- readLines(paste0(path_start, "correct/out-ex-1_gran-3"))
  test    <- sum_str(file_in = paste0(path_start, "raw_in/example-1.R"), dir_out = "", granularity = 3,
                     file_out = "object", last_sep = TRUE, title = FALSE)[[1]]
  expect_identical(correct, test)


### .. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
### check for granularity == 2
# sum_str(file_in = "example-1.R", dir_in = "tests/testthat/raw_in", granularity = 2,
# dir_out = "tests/testthat/correct", file_out = "out-ex-1_gran-2", title = FALSE)
  correct <- readLines(paste0(path_start, "correct/out-ex-1_gran-2"))
  test    <- sum_str(file_in = paste0(path_start, "raw_in/example-1.R"), dir_out = "", granularity = 2,
                     file_out = "object", last_sep = TRUE, title = FALSE)[[1]]
  expect_identical(correct, test)


### .. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
### check for granularity == 2
  # sum_str(file_in = "example-1.R", dir_in = "tests/testthat/raw_in", granularity = 1,
  # dir_out = "tests/testthat/correct", file_out = "out-ex-1_gran-1", title = FALSE)
  correct <- readLines(paste0(path_start, "correct/out-ex-1_gran-1"))
  test    <- sum_str(file_in = paste0(path_start, "raw_in/example-1.R"), dir_out = "", granularity = 1,
                     file_out = "object", last_sep = TRUE, title = FALSE)[[1]]
  expect_identical(correct, test)

})
context("granularity example-2")
test_that("granularity example-2", {
  # create originals
  # mapply(sum_str,
  #        file_in = "example-2.R", dir_in = "tests/testthat/raw_in",
  #        dir_out = "tests/testthat/correct", title = FALSE,
  #        granularity = 3:1,
  #        file_out = paste0("out-ex-2_gran-", 3:1))

### .. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
### check for granularity == 3
  correct <- readLines(paste0(path_start, "correct/out-ex-2_gran-3"))
  test    <- sum_str(file_in = paste0(path_start, "raw_in/example-2.R"), dir_out = "", granularity = 3,
                     file_out = "object", last_sep = TRUE, title = FALSE)[[1]]
  expect_identical(correct, test)


### .. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
### check for granularity == 2
  correct <- readLines(paste0(path_start, "correct/out-ex-2_gran-2"))
  test    <- sum_str(file_in = paste0(path_start, "raw_in/example-2.R"), dir_out = "", granularity = 2,
                     file_out = "object", last_sep = TRUE, title = FALSE)[[1]]
  expect_identical(correct, test)


### .. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
### check for granularity == 2
  correct <- readLines(paste0(path_start, "correct/out-ex-2_gran-1"))
  test    <- sum_str(file_in = paste0(path_start, "raw_in/example-2.R"), dir_out = "", granularity = 1,
                     file_out = "object", last_sep = TRUE, title = FALSE)[[1]]
  expect_identical(correct, test)

})
context("granularity example-3")

test_that("granularity example-3", {
  # create originals
  # mapply(sum_str,
  #        file_in = "example-3.R", dir_in = "tests/testthat/raw_in",
  #        dir_out = "tests/testthat/correct", title = FALSE,
  #        granularity = 3:1,
  #        file_out = paste0("out-ex-3_gran-", 3:1))

### .. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
### check for granularity == 3
  correct <- readLines(paste0(path_start, "correct/out-ex-3_gran-3"))
  expect_warning(test <- sum_str(file_in = paste0(path_start, "raw_in/example-3.R"), dir_out = "", granularity = 3,
                     file_out = "object", last_sep = TRUE, title = FALSE)[[1]])
  expect_identical(correct, test)


### .. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
### check for granularity == 2
  correct <- readLines(paste0(path_start, "correct/out-ex-3_gran-2"))
  expect_warning(test <- sum_str(file_in = paste0(path_start, "raw_in/example-3.R"), dir_out = "", granularity = 2,
                     file_out = "object", last_sep = TRUE, title = FALSE)[[1]])
  expect_identical(correct, test)


### .. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
### check for granularity == 1
# level does not exist:
  expect_warning(sum_str(file_in = paste0(path_start, "raw_in/example-3.R"), dir_out = "", granularity = 1,
                         file_out = "object", last_sep = TRUE, title = FALSE))

})

context("granularity example-4")
# generate examples
# note that level 4 does not have any comments and level 2 is missing
# mapply(sum_str,
#        file_in = "example-4.R", dir_in = "tests/testthat/raw_in",
#        dir_out = "tests/testthat/correct", title = FALSE,
#        granularity = 3:1,
#        file_out = paste0("out-ex-4_gran-", 3:1))
### .. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
### check for granularity == 3
  # expect warning and test value
test_that("granularity 4", {
  correct <- readLines(paste0(path_start, "correct/out-ex-4_gran-3"))
  expect_warning(test <- sum_str(file_in = paste0(path_start, "raw_in/example-4.R") , dir_out = "", granularity = 3,
                     file_out = "object", last_sep = TRUE, title = FALSE)[[1]])
  expect_identical(correct, test)


### .. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
### check for granularity == 2
  correct <- readLines(paste0(path_start, "correct/out-ex-4_gran-2"))
  expect_warning(test    <- sum_str(file_in = paste0(path_start, "raw_in/example-4.R"), dir_out = "", granularity = 2,
                     file_out = "object", last_sep = TRUE, title = FALSE)[[1]])
  expect_identical(correct, test)


### .. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
### check for granularity == 1
### .. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
### check for granularity == 3
  correct <- readLines(paste0(path_start, "correct/out-ex-3_gran-3"))
  expect_warning(test <- sum_str(file_in = paste0(path_start, "raw_in/example-3.R"), dir_out = "", granularity = 3,
                     file_out = "object", last_sep = TRUE, title = FALSE)[[1]])
  expect_identical(correct, test)


### .. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
### check for granularity == 2
  correct <- readLines(paste0(path_start, "correct/out-ex-3_gran-2"))
  expect_warning(test <- sum_str(file_in = paste0(path_start, "raw_in/example-3.R"), dir_out = "", granularity = 2,
                     file_out = "object", last_sep = TRUE, title = FALSE)[[1]])
  expect_identical(correct, test)


### .. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
### check for granularity == 1
  correct <- readLines(paste0(path_start, "correct/out-ex-4_gran-1"))
  expect_warning(test  <- sum_str(file_in = paste0(path_start, "raw_in/example-4.R"), dir_out = "", granularity = 1,
                     file_out = "object", last_sep = TRUE, title = FALSE)[[1]])
  expect_identical(correct, test)


})

context("last_sep = FALSE")
#   ____________________________________________________________________________
#   check 3 granularities last_sep = FALSE
# generate samples
# mapply(sum_str, file_in = paste0("example-", rep(1:5, each = 3), ".R"),
#        dir_in = "tests/testthat/raw_in", granularity = rep(1:3, 5),
# dir_out = "tests/testthat/correct",
# file_out = paste0("out-ex-", rep(1:5, each = 3), "_gran-", rep(1:3, 5), "_last_FALSE"), title = FALSE)

test_that("example-3-and-4", {
  # after having checked that files are actually correct:

  for (i in 1:3) {
    for (j in c(1, 2, 5)) {
      correct <- readLines(paste0(path_start, paste0("correct/out-ex-", j, "_gran-", i, "_last_FALSE")))
      test    <- sum_str(file_in = paste0(path_start, "raw_in/example-", j, ".R"), dir_out = "", granularity = i,
                         file_out = "object", last_sep = FALSE, title = FALSE)[[1]]
      expect_identical(correct, test)

    }
  } # loop over granularity

})
