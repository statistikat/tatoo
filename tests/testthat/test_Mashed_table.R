context('Mashed_table')

#* @testing mashed_table
#* @testing mash_table

tdat1 <- data.frame(
  numbers = c(1.434, 190.3, 228.311, 5.210, 4321543),
  animals = c('dog', 'cat', 'camel', 'pig', 'mouse'),
  factors = factor(c('rain', 'grain', 'stain', 'pain', 'main')),
  ints    = c(10L, 20L, 40L, 30L, 50L),
  stringsAsFactors = FALSE
)

tdat2 <- data.frame(
  numbers_xt = c(1, 290, 0.311, 0.210, 1000),
  animals_xt = c('god', 'tac', 'lemac', 'gip', 'esuom'),
  factors_xt = factor(c('tractor', 'hector', 'andrew', 'milli', 'vanilli')),
  ints    = c(5L, 5L, 10L, 30L, 25L),
  stringsAsFactors = FALSE
)

tdat3 <- data.frame(
  numbers_xt = factor(c(1, 290, 0.311, 0.210, 1000)),
  animals_xt = factor(c('god', 'tac', 'lemac', 'gip', 'esuom')),
  factors_xt = factor(c('tractor', 'hector', 'andrew', 'milli', 'vanilli')),
  ints       = c('god', 'tac', 'lemac', 'gip', 'esuom'),
  stringsAsFactors = FALSE
)


test_that('mash_table: stacking tables by row works', {
  #* @testing mash_table
  #* @testing mash_rows
  #* @testing as.data.table.Mashed_table
  #* @testing as.data.frame.Mashed_table

  # Creating mash tables works for two elements (legacy version of the function
  # only accepted two arguments)
    expect_error(mash_table(tdat1, tdat2))
    expect_silent(st1 <- mash_table(tdat1, tdat2, rem_ext = '_xt'))
    expect_silent(st2 <- mash_table(tdat1, tdat3, rem_ext = '_xt'))

  # but also for an arbitrary number
    expect_error(mash_table(tdat1, tdat2, tdat1, tdat2))
    expect_silent(st3 <- mash_table(tdat1, tdat2, tdat1, tdat2, rem_ext = '_xt'))
    expect_silent(st4 <- mash_table(tdat1, tdat3, tdat1, tdat2, rem_ext = '_xt'))

  # Testing if row mashing works
    ## Input must be a mash_table
      expect_error(mash_rows(tdat1))

    ## for two data.frames
      expect_silent(res1 <- mash_rows(st1))
      expect_identical(nrow(res1), sum(unlist(lapply(st1, nrow))))

    ## for an arbitrary number of data.frames
      expect_silent(res3 <- mash_rows(st3))
      expect_identical(nrow(res3), sum(unlist(lapply(st3, nrow))))

    ## Ensure classes were preserved
      expect_identical(lapply(res1, class), lapply(tdat1, class))

    ## For better visual differentiation, blank rows can be inserted between
    ## mashes (useful especially for latex or xlsx export)
      expect_silent(res1 <- as.data.table(st1, insert_blank_row = TRUE))

      sel <- seq(3, nrow(res1), 3)
      expect_true(all(rowSums(res1[sel] == '') == ncol(res1)))
      expect_false(any(rowSums(res1[-sel] == '') == ncol(res1)))
})


test_that('mash_table: stacking tables by col works', {
  #* @testing mash_cols
  #* @testing as.data.table.Mashed_table

  # Creating stack tables
  expect_silent(st1 <- mash_table(tdat1, tdat2, rem_ext = '_xt'))
  expect_silent(st2 <- mash_table(tdat1, tdat3, rem_ext = '_xt'))
  expect_silent(st3 <- mash_table(tdat1, tdat2, tdat1, tdat2, rem_ext = '_xt'))

  # Create col stacked data.table
  expect_error(mash_cols(tdat1))
  expect_error(mash_cols(st1, suffixes = c('.x', '.y', '.z')))
  expect_silent(res1 <- mash_cols(st1))

  # Create col stacked data.table (with id_vars)
  st1[[1]]$id_1 <- LETTERS[1:5]
  st1[[2]]$id_1 <- LETTERS[1:5]
  st1[[1]]$id_2 <- letters[6:10]
  st1[[2]]$id_2 <- letters[6:10]

  expect_silent(res1 <- mash_cols(
    st1, id_vars = c('id_1', 'id_2')
  ))
  expect_silent(res2 <- mash_cols(
    st1,
    id_vars = c('id_1', 'id_2'),
    suffixes = c('foo', 'bar')
  ))
  expect_silent(res3 <- mash_cols(st1))
  expect_silent(res4 <- mash_cols(
    st1,
    suffixes = c('.x', '.y')
  ))


  expect_identical(unlist(
    lapply(res1, class), use.names = FALSE),
    c("character", "character", "numeric", "numeric", "character",
      "character", "factor", "factor", "integer", "integer")
  )

  expect_identical(
    names(res2),
    c("id_1", "id_2", "numbersfoo", "numbersbar", "animalsfoo", "animalsbar",
      "factorsfoo", "factorsbar", "intsfoo", "intsbar")
  )

  expect_identical(
    names(res3),
    c("numbers", "numbers", "animals", "animals", "factors", "factors",
      "ints", "ints", "id_1", "id_1", "id_2", "id_2")
  )

  expect_identical(
    names(res4),
    c("numbers.x", "numbers.y", "animals.x", "animals.y", "factors.x",
      "factors.y", "ints.x", "ints.y", "id_1.x", "id_1.y", "id_2.x",
      "id_2.y")
  )

  # test for an arbitrary number of columns
  st3id <- lapply(st3, function(x) {
    x$id  <- seq_len(nrow(x))
    x$id2 <- LETTERS[x$id]
    return(x)
  }) %>% mash_table_list()

  expect_silent(mash_cols(st3id))
  expect_silent(mash_cols(st3id, id_vars = c('id', 'id2')))
})



test_that('mash_table: as.data.table and setters work', {
  #* @testing as.data.table.Mashed_table
  #* @testing as.data.frame.Mashed_table

  expect_silent(st1 <- mash_table(tdat1, tdat2, rem_ext = '_xt'))
  st1[[1]]$id_1 <- LETTERS[1:5]
  st1[[2]]$id_1 <- LETTERS[1:5]
  st1[[1]]$id_2 <- letters[6:10]
  st1[[2]]$id_2 <- letters[6:10]

  # See if as.data.table outputs the correct number of rows
    expect_identical(
      nrow(as.data.table(st1)),
      10L
    )

    expect_identical(
      length(capture.output(print(st1))),
      11L
    )


  # Default parameters can be overwritten in as.data.table or modifed via set
    tres1 <- as.data.table(st1, insert_blank_row = TRUE)

    expect_identical(
      nrow(tres1),
      14L
    )

    insert_blank_row(st1) <- TRUE

    expect_identical(
      as.data.table(st1),
      tres1
    )

    expect_identical(
      length(capture.output(print(st1))),
      15L
    )

  # Set stack method
    mash_method(st1) <- 'col'
    expect_identical(
      nrow(as.data.table(st1)),
      5L
    )

  # set by
    id_vars(st1) <- c('id_1', 'id_2')

    expect_identical(
      nrow(as.data.table(st1)),
      5L
    )

    expect_identical(
      length(capture.output(print(st1))),
      6L
    )

    expect_identical(
      as.data.frame(st1),
      as.data.frame(as.data.table(st1))
    )

    expect_identical(
      as.data.table(as.data.frame(st1)),
      as.data.table(st1)
    )
})


test_that('rmash and cmash behave as expected', {
  expect_silent(st1 <- mash_table(tdat1, tdat2, rem_ext = '_xt'))
  st1[[1]]$id_1 <- LETTERS[1:5]
  st1[[2]]$id_1 <- LETTERS[1:5]
  st1[[1]]$id_2 <- letters[6:10]
  st1[[2]]$id_2 <- letters[6:10]


  tres1 <- rmash(st1)
  tres2 <- rmash(st1[[1]], st1[[2]], insert_blank_row = TRUE)

  expect_identical(nrow(tres1), 10L)
  expect_identical(nrow(tres2), 14L)
  expect_identical(class(tres1), c('data.table', 'data.frame'))


  tres3 <- cmash(st1)
  tres4 <- cmash(st1[[1]], st1[[2]], id_vars = c('id_1', 'id_2'))

  expect_identical(
    ncol(tres3),
    sum(sapply(st1, ncol))
  )

  expect_identical(
    ncol(tres4),
    sum(sapply(st1, ncol)) - 2L
  )
})

