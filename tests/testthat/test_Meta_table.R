context("Meta_table")


test_that("make_meta_table_print_title works as expected", {
  tmeta <- ttmeta(
    table_id = 't001',
    title = 'Table 1',
    longtitle = 'Table of Numbers',
    subtitle = 'A table that contains numbers but maybe also letters',
    footer = 'a footer'
  )

  tmeta2 <- ttmeta(
    table_id = 't001',
    title = 'Table 1',
    longtitle = 'Table of Numbers'
  )

  tres1  <- make_meta_table_print_title(tmeta)
  tres1b <- make_meta_table_print_title(tmeta, subtitle = FALSE)
  tres2  <- make_meta_table_print_title(tmeta2)

  expect_identical(
    tres1,
    "t001: Table 1 - Table of Numbers\nA table that contains numbers but maybe also letters"
  )

  expect_identical(
    tres1b, tres2
  )

  expect_identical(
    tres2,
    "t001: Table 1 - Table of Numbers"
  )
})



test_that("make_meta_table_print_title works as expected", {
  tmeta <- ttmeta(
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

  expect_silent(tres <- meta_table(tdat, tmeta))

  ## manual tests

  # tf <- file.path(tempdir(), 'pub.xlsx')
  # save_xlsx(tres, tf, overwrite = TRUE)
  # hammr::excel(tf)


})


