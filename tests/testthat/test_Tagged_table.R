context("Tagged_table")


test_that("make_tag_table_print_title works as expected", {
  tmeta <- tt_meta(
    table_id = 't001',
    title = 'Table 1',
    longtitle = 'Table of Numbers',
    subtitle = 'A table that contains numbers but maybe also letters',
    footer = 'a footer'
  )

  tmeta2 <- tt_meta(
    table_id = 't001',
    title = 'Table 1',
    longtitle = 'Table of Numbers'
  )

  tres1  <- make_tag_table_print_title(tmeta)
  tres1b <- make_tag_table_print_title(tmeta, show_subtitle = FALSE)
  tres2  <- make_tag_table_print_title(tmeta2)

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

test_that("make_tag_table_print_title works as expected", {
  tmeta <- tt_meta(
    table_id = 't001',
    title = 'Table 1',
    longtitle = 'Table of Numbers',
    subtitle = c('A table that contains numbers but maybe also letters', 'and can span several rows'),
    footer = c('a footer', 'with many rows', '(c) someone')
  )

  tdat <- data.frame(
    x = letters[1:5],
    y = letters[10:14]
  )

  expect_silent(tres <- tag_table(tdat, tmeta))

  ## manual tests
  # tf <- file.path(tempdir(), 'pub.xlsx')
  # save_xlsx(tres, tf, overwrite = TRUE)
  # hammr::excel(tf)
})


test_that("Creating tag_table works as expected", {
df1 <- iris %>%
  dplyr::group_by(Species) %>%
  dplyr::summarise(
    length = mean(Sepal.Length),
    width  = mean(Sepal.Width)) %>%
  hammr::df_round(2)

df2 <- iris %>%
  dplyr::group_by(Species) %>%
  dplyr::summarise(
    length = sd(Sepal.Length),
    width  = sd(Sepal.Width)) %>%
  hammr::df_round(2)

tmeta <- tt_meta(
  table_id = 't001',
  title = 'Table 1',
  longtitle = 'Table of Numbers',
  subtitle = c('A table that contains numbers but maybe also letters', 'and can span several rows'),
  footer = c('a footer', 'with many rows', '(c) someone')
)

tres1 <- tag_table(df1, tmeta)

# A meta table should be a data.table
  expect_identical(
    class(tres1),
    c("Tagged_table", "Tatoo_table", "data.table", "data.frame")
  )

# Creating a meta-table from a meta table should replace the meta-attribute
# without awkward stuff happening like a duplicate class attribute or such
  newmeta <- tt_meta('t1', 'a table')

  tres2 <- tag_table(tres1, meta = newmeta)
  expect_identical(
    class(tres2),
    c("Tagged_table", "Tatoo_table", "data.table", "data.frame")
  )

  expect_identical(
    attr(tres2, 'meta'),
    newmeta
  )

  # Alternatively metadata can also be assigned like this:
  tres3 <- tres1
  meta(tres3) <- newmeta

  expect_identical(
    tres3,
    tres2
  )
})



test_that("metadata replacement functions work", {
  tdat <- data.frame(
    x = letters[1:5],
    y = letters[10:14]
  )

  tres <- assign_tt_meta(
    tdat,
    list(longtitle = 'blubb')
  )

  expect_identical(
    class(tres),
    c("Tagged_table", 'Tatoo_table', "data.table", "data.frame")
  )

  expect_identical(
    attr(tres, 'meta')$longtitle, 'blubb'
  )

  table_id(tres) <- 'T01'
  title(tres) <- 'Table 01'
  longtitle(tres) <- 'The first table'
  subtitle(tres) <- c(
    'Another awkward subtitle for a table',
    'that can span multiple lines'
  )
  footer(tres) <- c(
    'a footer can also',
    'span multiple lines'
  )

  expect_identical(
    class(tres),
    c("Tagged_table", 'Tatoo_table', "data.table", "data.frame")
  )

  expected_meta <- tt_meta(
    table_id = "T01",
    title = "Table 01",
    longtitle = "The first table",
    subtitle = c(
      "Another awkward subtitle for a table",
      "that can span multiple lines"),
    footer = c(
      "a footer can also",
      "span multiple lines")
  )

  expect_identical(
    attr(tres, 'meta'),
    expected_meta
  )

  title(tres) <- NULL

  meta(tres) <- NULL


  expect_identical(
    class(tres),
    c('Tatoo_table', 'data.table', 'data.frame')
  )




})
