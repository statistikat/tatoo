context("Comp_table")


test_that("Comp_table works as expected", {
  tdat <- list()
  for(i in seq_len(3)){
    tdat[[i]] <- data.frame(
      small = letters[i:(i+5)],
      tall = LETTERS[i:(i+5)]
    )
  }


  # titles must be specified (otherwise comp_table would just be a wrapper for
  # cbdind)
  expect_error(tres <- comp_table(tdat))

  # Test that comp table does not fail on legal input
  expect_silent(tres <- comp_table(
    tdat,
    titles = c('tab1', 'tab2', 'tab3')
  ))
  expect_s3_class(tres, 'Comp_table')


  # xlsx output should work
  td <- tempdir()
  wb <- as_workbook(tres)


  outfile <- file.path(td, 'comp_table.xlsx')
  expect_silent(openxlsx::saveWorkbook(wb, outfile, overwrite = TRUE))
})

