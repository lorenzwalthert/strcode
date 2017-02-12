path_start <- NULL
#path_start <- "tests/testthat/"

# test dir_in NULL should summarize multiple files
# to console
sum_str(dir_in = paste0(path_start, "raw_in"), file_in = NULL,
        file_out = "object",
        width = 40,
        granularity = 2,
        last_sep = FALSE,
        header = TRUE)

args <- list(dir_in = paste0(path_start, "raw_in"), file_in = NULL,
          file_out = "",
          width = 40,
          granularity = 2,
          last_sep = FALSE,
          header = TRUE)

do.call("sum_str", args)

# to file
sum_str(dir_in = paste0(path_start, "raw_in"), file_in = NULL,
        file_out = NULL, dir_out = paste0(path_start, "test-dir_in"),
        width = 40,
        granularity = 2,
        last_sep = FALSE,
        header = TRUE)


##  ............................................................................
##  actual tests                                                            ####
context("dir_in: multiple files")
test_that("equal", {
  expect_true(check_via_read(path_start = path_start))
})
