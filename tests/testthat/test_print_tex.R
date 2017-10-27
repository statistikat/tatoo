context("print_tex")


test_that("print_tex works as expected", {
  source(file.path(test_path(), 'testdata', 'testdata.R'))

  print_tex(t_mash_1)


})
