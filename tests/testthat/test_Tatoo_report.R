context("Tatoo_report")

#* @testing compile_report
#* @testing tatoo_report


test_that("Tatoo_report works as expected", {
  tdat <- list()

  for(i in seq_len(3)){
    meta <- tt_meta(
      paste0('T', i),
      'A table',
      'With a Long Title'
    )

    tdat[[i]] <- tag_table(
      data.frame(
        small = letters[i:(i+3)],
        big = LETTERS[i:(i+3)]
      ),
      meta = meta
    )
  }

  expect_silent(tdat <- tatoo_report(tdat))
  td <- tempdir()


  tfile_xlsx <- file.path(td, 'pr.xlsx')
  expect_silent(
    save_xlsx(tdat, tfile_xlsx, overwrite = TRUE)
  )
})




test_that("Tatoo_report print method", {
#* @testing print.Tatoo_report
#* @testing compile_report
#* @testing compile_report_list

  tm <- tt_meta(
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
  expect_silent(tmeta <- tag_table(tdat, meta = tm))


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
  expect_silent(tmash <- mash_table(
    tdat1, tdat2, meta = tt_meta('t02', 'a mash table'))
  )

  tdat <- list()
  for(i in seq_len(3)){
    tdat[[i]] <- data.frame(
      id = 1:6,
      small = letters[i:(i+5)],
      tall = LETTERS[i:(i+5)]
    )
  }
  tcomp <- comp_table_list(
    tdat,
    table_names = c('tab1', 'tab2', 'tab3'),
    meta = tt_meta('t03', 'a comp table')
  )

  tstack <- stack_table(tmeta, tcomp, tmash, tdat1, meta = tm)
  tres <- compile_report(tcomp, tdat1, tmeta, tstack, tmeta, tmash)

  expect_output(print(tres))
})
