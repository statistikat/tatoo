context("save_xlsx")

outdir <- file.path(test_path(), 'test_out')

#* @testing save_xlsx
#* @testing save_xlsx.default
#* @testing as_workbook.Tatoo_report
#* @testing as_workbook.default
#* @testing write_worksheet.Tagged_table
#* @testing write_worksheet
#* @testing write_worksheet.default


test_that('save_xlsx mash_table', {
  #* @testing write_worksheet.Mashed_table

  tdat1 <- data.frame(
    numbers = c(1.434, 190.3, 228.311, 5.210, 4321543),
    animals = c('dog', 'cat', 'camel', 'pig', 'mouse'),
    factors = factor(c('rain', 'grain', 'stain', 'pain', 'main')),
    ints    = c(10L, 20L, 40L, 30L, 50L),
    stringsAsFactors = FALSE
  )

  tm <- tt_meta(
    table_id = 'tid',
    title = 'title',
    longtitle = 'longitle',
    subtitle = 'subtitle',
    footer = ' ---------------- '
  )


  expect_silent(st1 <- mash_table(tdat1, tdat1, tdat1, tdat1))

  expect_silent(st2 <- mash_table(
    tdat1, tdat1, tdat1,
    mash_method = 'col'
  ))

  expect_silent(st3 <- mash_table(
    tdat1, tdat1, tdat1,
    mash_method = 'col',
    id_vars = 'factors'
  ))

  expect_silent(st4 <- mash_table(
    tdat1, tdat1, tdat1,
    mash_method = 'col',
    id_vars = 'factors',
    sep_height = 50,
    meta = tm
  ))


  expect_silent(tres <- compile_report(
    row = st1,
    col = st2,
    colby = st3,
    meta = st4
  ))

  of <- file.path(outdir, 'mash_table.xlsx')
  save_xlsx(tres, of, overwrite = TRUE)
  # openxlsx::openXL(of)
  # hammr::excel(of2)
})


test_that("xlsx output for comp_tables works", {
  #* @testing write_worksheet.Composite_table

  # Generate test data
  tdat <- list()
  for(i in seq_len(3)){
    tdat[[i]] <- data.frame(
      id = 1:6,
      small = letters[i:(i+5)],
      tall = LETTERS[i:(i+5)]
    )
  }

  names(tdat) <- c('tab1', 'tab2', 'tab3')

  tm <- tt_meta(
    table_id = 'tid',
    title = 'title',
    longtitle = 'longitle',
    subtitle = 'subtitle',
    footer = ' ---------------- '
  )


  # Ursula wants to export her comp_table as .xlsx
  # (the xlsx files have to be checked manually)
  expect_silent(ct1 <- comp_table_list(tdat))

  expect_silent(ct2 <- comp_table_list(
    tdat,
    id_vars = 'id'
  ))

  expect_silent(ct3 <- comp_table_list(
    tdat,
    id_vars = 'id',
    meta = tm
  ))

  tres <- compile_report(
    normal = ct1,
    id_vars = ct2,
    meta = ct3
  )

  of <- file.path(outdir, 'comp_table.xlsx')
  save_xlsx(tres, of, overwrite = TRUE)
  # openxlsx::openXL(of)
  # hammr::excel(of2)
})



test_that("xlsx output for stack_tables works", {
  #* @testing write_worksheet.Stacked_table

  # Generate test data
  tm <- tt_meta(
    table_id = 'tid',
    title = 'title',
    longtitle = 'longitle',
    subtitle = 'subtitle',
    footer = ' ---------------- '
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

  names(tdat) <- c('tab1', 'tab2', 'tab3')

  tcomp <- comp_table_list(
    tdat,
    meta = tt_meta('t03', 'a comp table')
  )


  # Constructor works
  sk1 <- stack_table(tmash, tcomp, tmeta, meta = tt_meta(
    table_id = 'rp1',
    title = 'stack table 1',
    longtitle = c('stack table 1 is a stack of tables', 'with a very long title', 'that spans several rows'),
    subtitle = 'with a subtitle',
    footer = c('that has a footer also', 'which goes over man lines')
  ))

  sk2 <- sk1
  spacing(sk2) <- 7

  tres <- compile_report(
    normal = sk1,
    spacing = sk2
  )

  of <- file.path(outdir, 'stack_table.xlsx')
  save_xlsx(tres, of, TRUE)

  # openxlsx::openXL(of)
  # hammr::excel(of)
})


