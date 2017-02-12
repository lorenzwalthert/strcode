path_start <- NULL
# path_start <- "tests/testthat/"

# the correct versions are generated after setwd to test/testthat
# with the below code and renamed

##  ............................................................................
##  create summaries                                                        ####

args <- list(path_in = paste0(path_start, "raw_in"), file_in = NULL,
          file_out = NULL, dir_out = paste0(path_start, "test-dir_in"),
          width = 40,
          granularity = 2,
          last_sep = FALSE,
          header = TRUE,
          file_in_extension = NULL)

test_that("call sum_str for side effects, yields warning", {
  expect_warning(do.call("sum_str", args))
})


##  ............................................................................
##  run tests on the summaries                                              ####
context("dir_in: multiple files")
test_that("equal", {
  for (i in c(1, 2, 4, 5)) { # 3 can't be tested since there is no summary
    expect_true(check_via_read(path_start = path_start,
                               directory = "test-dir_in",
                               filename = paste0("code_summary-example-", i)
                               ))
  }
})


##  ............................................................................
##  misspecified dir_in                                                     ####

context("dir_in: errors")
test_that("misspecified dir_in throws error", {
  args <- list(path_in = paste0(path_start, "wrong_dir_in"), file_in = NULL,
               file_out = NULL, dir_out = paste0(path_start, "test-dir_in"),
               width = 40,
               granularity = 2,
               last_sep = FALSE,
               header = TRUE)
  expect_error(do.call("sum_str", args))

})

