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

  ## multinames must be specified (otherwise comp_table would just be a wrapper for
  ## cbind)
    expect_error(tres <- comp_table(tdat[[1]], tdat[[2]], tdat[[3]]))
    expect_error(tres <- comp_table_list(tdat))

  ## If "tables" is a named list, multinames are automatically set to element names
    names(tdat) <- c('a', 'b', 'c')
    expect_silent(tres <- comp_table_list(tdat))
    expect_s3_class(tres, 'Comp_table')

  ## Alternatively, table names can be specified manually
    names(tdat) <- NULL
    expect_silent(tres <- comp_table_list(
      tdat,
      multinames = c('tab1', 'tab2', 'tab3')
    ))
    expect_s3_class(tres, 'Comp_table')


  # Ursulas table have an ID column. She wants to merge them, instead of
  # cbinding them. This has the advantage of avoiding duplicate ID columns
  # and ensuring data integrity
    expect_silent(tres <- comp_table(
      tdat[[1]], tdat[[2]], tdat[[3]],
      multinames = c('tab1', 'tab2', 'tab3'),
      by = 'id'
    ))

  # display print (manual check)
    # names(attr(tres, 'multinames'))[[3]] <- 'a very long title, very long'
    # print(tres)

})


test_that("as.data.table.Comp_table works as expected", {
  tl <- list()
  for(i in seq_len(3)){
    tl[[i]] <- data.frame(
      id = 1:6,
      small = letters[i:(i+5)],
      tall = LETTERS[i:(i+5)]
    )
  }

  tdat1 <- comp_table_list(
    tl,
    c('tab1', 'tab2', 'tab3')
  )

  expect_identical(
    names(as.data.table(tdat1)),
    c("tab1.id", "tab1.small", "tab1.tall", "tab2.id", "tab2.small",
      "tab2.tall", "tab3.id", "tab3.small", "tab3.tall")
  )

  expect_identical(
    names(as.data.table(tdat1, multinames = FALSE)),
    names(tdat1)
  )

  tdat2 <- comp_table_list(
    tl,
    c('tab1', 'tab2', 'tab3'),
    by = 'id'
  )

  expect_identical(
    names(as.data.table(tdat2)),
    c("id", "tab1.small", "tab1.tall", "tab2.small", "tab2.tall",
      "tab3.small", "tab3.tall")
  )
})
