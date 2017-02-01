context("Pub_table")


test_that("make_pub_table_print_title works as expected", {
  tmeta <- pub_tableMeta(
    tableId = 't001',
    title = 'Table 1',
    longtitle = 'Table of Numbers',
    subtitle = 'A table that contains numbers but maybe also letters'
  )

  tmeta2 <- pub_tableMeta(
    tableId = 't001',
    title = 'Table 1',
    longtitle = 'Table of Numbers'
  )

  tres1  <- make_pub_table_print_title(tmeta)
  tres1b <- make_pub_table_print_title(tmeta, subtitle = FALSE)
  tres2  <- make_pub_table_print_title(tmeta2)

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
