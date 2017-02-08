context('mash_table')

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

  # Creating mash tables works for two elements (legacy version of the function
  # only accepted two arguments)
    expect_error(mash_table(tdat1, tdat2))
    expect_silent(st1 <- mash_table(tdat1, tdat2, rem_ext = '_xt'))
    expect_silent(st2 <- mash_table(tdat1, tdat3, rem_ext = '_xt'))

  # but also for an arbitrary number
    expect_error(mash_table(tdat1, tdat2, tdat1, tdat2))
    expect_silent(st3 <- mash_table(tdat1, tdat2, tdat1, tdat2, rem_ext = '_xt'))
    expect_silent(st4 <- mash_table(tdat1, tdat3, tdat1, tdat2, rem_ext = '_xt'))


  #* @testing mash_rows
  #* @testing as.data.table.Mash_table

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
      expect_error(as.data.table(st1, mash_method = 'col', insert_blank_row = TRUE))

      sel <- seq(3, nrow(res1), 3)
      expect_true(all(rowSums(res1[sel] == '') == ncol(res1)))
      expect_false(any(rowSums(res1[-sel] == '') == ncol(res1)))
})


test_that('mash_table: stacking tables by col works', {
  #* @testing mash_cols
  #* @testing as.data.table.Mash_table

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
    st1, by = c('id_1', 'id_2')
  ))
  expect_silent(res2 <- mash_cols(
    st1,
    by = c('id_1', 'id_2'),
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
  }) %>% as_mash_table()

  expect_silent(mash_cols(st3id))
  expect_silent(mash_cols(st3id, by = c('id', 'id2')))
})



test_that('printing as latex works', {
  #* @testing mash_rows_tex
  #* @testing print_tex

  expect_silent(st1 <- mash_table(tdat1, tdat2, rem_ext = '_xt'))
  expect_silent(st2 <- mash_table(tdat1, tdat3, rem_ext = '_xt'))

  res1 <- mash_rows(st1)
  res2 <- mash_rows(st2)

  expect_silent(mash_rows_tex(st1, insert_blank_row = TRUE))
  expect_silent(mash_rows_tex(st1, insert_blank_row = FALSE))
  expect_output(print_tex(st1, mash_method = 'row'))
  expect_output(print_tex(st2))
})



test_that('exporting as xlsx works', {
  expect_silent(st1 <- mash_table(tdat1, tdat2, rem_ext = '_xt'))
  of <- file.path(test_path(), 'test_out', 'mash_table.xlsx')
  save_xlsx(st1, of, overwrite = TRUE)
  #hammr::excel(of)


  of2 <- file.path(test_path(), 'test_out', 'mash_table_meta.xlsx')
  st1_meta <- pub_table(
    st1,
    pub_table_meta(
      table_id = 'tid',
      title = 'title',
      longtitle = 'longitle',
      subtitle = 'subtitle',
      footer = ' ---------------- ')
  )
  save_xlsx(st1_meta, of2, overwrite = TRUE)
  #hammr::excel(of2)



  # save_xlsx.StackTable(st1, '/home/hoelk/blah.xlsx', overwrite = TRUE, startRow = 10)
  # save_xlsx.StackTable(st1, '/home/hoelk/blah.xlsx', overwrite = TRUE, xy = c(6, 10))
  # save_xlsx.StackTable(st1, '/home/hoelk/blah.xlsx', overwrite = TRUE, sep_height = 24, xy = c(6, 10))
  # save_xlsx.StackTable(st1, '/home/hoelk/blah.xlsx', overwrite = TRUE, sep_height = 24, xy = c(6, 10), insert_blank_row = TRUE)
})
