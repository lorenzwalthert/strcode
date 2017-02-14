# test for a file
expect_equal(
  fn <- sum_str(dir_in = "./placeholder_code",
        file_in = "example.R",
        file_out = "",
        width = 40,
        granularity = 2,
        lowest_sep = TRUE,
        header = TRUE),
  a <- "line  level section
  2	#   _
  3	#   function test
  5	##  .
  6	##  -A: pre-processing
  56	##  ....................................
  57	##  B: actual function
  81	##  ....................................
  83	#   ____________________________________
  84	#   function test2
  86	##  ....................................
  87	##  A: pre-processing
  137	##  ....................................
  138	##  B: actual function")



# test for a directory
sum_str(dir_in = "~/datasciencecoursera/refactor/R",
        file_in = NULL,
        file_out = "",
        width = 40,
        granularity = 2,
        lowest_sep = TRUE,
        header = TRUE)


# test for full path in file_in
sum_str(dir_in = NULL,
        file_in = "~/datasciencecoursera/refactor/R/cfactor.R",
        dir_out = "",
        width = 10,
        granularity = 2,
        lowest_sep = TRUE,
        header = TRUE)


# test for full path in file_in
sum_str(dir_in = NULL,
        file_in = rstudioapi::getSourceEditorContext()$path,
        dir_out = "",
        width = 40,
        granularity = 2,
        lowest_sep = TRUE,
        header = TRUE)


# test for full path in file_in
sum_str(dir_in = NULL,
        file_in = "placeholder_code/example.R",
        dir_out = "",
        width = 40,
        granularity = 2,
        lowest_sep = TRUE,
        header = TRUE)


# write to file (one file)
sum_str(dir_in = NULL,
        file_in = "placeholder_code/example.R",
        dir_out = "placeholder_code",
        file_out_extension = ".txt",
        width = 40,
        granularity = 2,
        lowest_sep = TRUE,
        header = TRUE)

# write to file (one file)
sum_str(dir_in = "placeholder_code",
        file_in =  NULL,
        file_in_extension = ".R",
        dir_out = "placeholder_code",
        file_out_extension = ".txt",
        width = 40,
        granularity = 2,
        lowest_sep = TRUE,
        header = TRUE)
