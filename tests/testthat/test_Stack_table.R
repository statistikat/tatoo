context("Stack_table")


test_that("Stack_table works as expected", {

  # Generate test data
  tmeta <- tt_meta(
    table_id = 't001',
    title = 'Table 1',
    longtitle = 'Table of Numbers',
    subtitle = 'A table that contains numbers but maybe also letters',
    footer = 'a footer'
  )

  tdat <- data.frame(
    x = letters[1:5],
    y = letters[10:14]
  )

  expect_silent(tpub <- meta_table(tdat, tmeta))


  tdat1 <- data.frame(
    numbers = c(1.434, 190.3, 228.311, 5.210, 4321543),
    animals = c('dog', 'cat', 'camel', 'pig', 'mouse'),
    stringsAsFactors = FALSE
  )

  tdat2 <- data.frame(
    numbers = c(1, 290, 0.311, 0.210, 1000),
    animals = c('god', 'tac', 'lemac', 'gip', 'esuom'),
    stringsAsFactors = FALSE
  )


  expect_silent(tmash <- mash_table(tdat1, tdat2))


  tdat <- list()
  for(i in seq_len(3)){
    tdat[[i]] <- data.frame(
      id = 1:6,
      small = letters[i:(i+5)],
      tall = LETTERS[i:(i+5)]
    )
  }




})
