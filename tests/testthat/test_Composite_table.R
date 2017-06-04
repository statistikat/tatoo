context("Composite_table")
#* @testing comp_table
#* @testing composite_table

test_that("Composite_table works as expected", {
  #* @testing comp_table
  #* @testing comp_table_list

  source(file.path(test_path(), 'testdata', 'testdata.R'))


  # Ursula wants to combine several tables to a Composite_table

  ## If no table names are specified, the names are derived from the R objects.
    expect_silent(tres <- comp_table(t_df1, t_df2, t_df3))
    expect_identical(
      names(attr(tres, 'multinames')),
      c("t_df1", "t_df2", "t_df3")
    )

  ## Custom table names can be specifed by supplying named arguments...
    expect_silent(tres <- comp_table(a = t_df1, b = t_df2, c = t_df3))
    expect_identical(
      names(attr(tres, 'multinames')),
      c("a", "b", "c")
    )

  ## ... or by supplying a named list to the list constructor
    expect_silent(tres <- comp_table_list(list(
      a = t_df1, b = t_df2, c = t_df3))
    )
    expect_identical(
      names(attr(tres, 'multinames')),
      c("a", "b", "c")
    )

  ## Passing a list without names to comp_table_list will enumerate the tables
    expect_silent(tres <- comp_table_list(list(
      t_df1, t_df2, t_df3))
    )
    expect_identical(
      names(attr(tres, 'multinames')),
      c("tab 1", "tab 2", "tab 3")
    )


  # Ursulas table have an ID column. She wants to merge them, instead of
  # cbinding them. This has the advantage of avoiding duplicate ID columns
  # and ensuring data integrity
    expect_silent(tres <- comp_table(
      tab1 = t_df1,
      tab2 = t_df1,
      tab3 = t_df1,
      id_vars = 'animals'
    ))
    expect_identical(
      ncol(tres),
      ncol(t_df1) * 3L - 3L + 1L
    )
})




test_that("Composite_table names get assigned correctly", {
  tmp <- list()
  for(i in seq_len(3)){
    tmp[[i]] <- data.frame(
      id = 1:6,
      small = letters[i:(i+5)]
    )
  }
  names(tmp) <- c('tab1', 'tab2', 'tab3')

  t_comp_1 <- comp_table_list(tmp)

  expect_identical(
    names(t_comp_1),
    c("id", "small", "id", "small", "id", "small")
  )
})



test_that("as.data.table.Composite_table works as expected", {
  #* @testing as.data.frame.Composite_table
  #* @testing as.data.table.Composite_table

  source(file.path(test_path(), 'testdata', 'testdata.R'))

  expect_identical(
    names(data.table::as.data.table(t_comp_1, multinames = TRUE)),
    c("tab1.id", "tab1.small", "tab1.tall", "tab2.id", "tab2.small",
      "tab2.tall", "tab3.id", "tab3.small", "tab3.tall")
  )

  expect_identical(
    names(data.table::as.data.table(t_comp_1, multinames = FALSE)),
    names(t_comp_1)
  )

  expect_identical(
    names(data.table::as.data.table(t_comp_2)),
    c("id", "tab1.small", "tab1.tall", "tab2.small", "tab2.tall",
      "tab3.small", "tab3.tall")
  )

  expect_identical(
    names(data.table::as.data.table(t_comp_2, multinames = FALSE)),
    names(t_comp_2)
  )
})
