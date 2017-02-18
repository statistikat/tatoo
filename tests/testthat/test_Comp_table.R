context("Comp_table")



test_that("Comp_table works as expected", {
  #* @testing comp_table
  #* @testing comp_table_list
  # Generate test data
    tdat <- list()
    for(i in seq_len(3)){
      tdat[[i]] <- data.frame(
        id = 1:6,
        small = letters[i:(i+5)],
        tall = LETTERS[i:(i+5)]
      )
    }

  # Ursula wants to combine several tables to a Comp_table (side-by-side tables)

  ## Titles must be specified (otherwise comp_table would just be a wrapper for
  ## cbind)
    expect_error(tres <- comp_table(tdat[[1]], tdat[[2]], tdat[[3]]))
    expect_error(tres <- comp_table_list(tdat))

  ## If "tables" is a named list, titles are automatically set to element names
    names(tdat) <- c('a', 'b', 'c')
    expect_silent(tres <- comp_table_list(tdat))
    expect_s3_class(tres, 'Comp_table')

  ## Alternatively, table names can be specified manually
    names(tdat) <- NULL
    expect_silent(tres <- comp_table_list(
      tdat,
      titles = c('tab1', 'tab2', 'tab3')
    ))
    expect_s3_class(tres, 'Comp_table')


  # Ursulas table have an ID column. She wants to merge them, instead of
  # cbinding them. This has the advantage of avoiding duplicate ID columns
  # and ensuring data integrity
    expect_silent(tres <- comp_table(
      tdat[[1]], tdat[[2]], tdat[[3]],
      titles = c('tab1', 'tab2', 'tab3'),
      by = 'id'
    ))
})
