context("save_xlsx")

outdir <- file.path(test_path(), 'test_out')


test_that('save_xlsx mash_table', {
  #* @testing save_xlsx.Mash_table
  #* @testing write_worksheet.Mash_table
  #* @testing as_workbook.Mash_table

  tdat1 <- data.frame(
    numbers = c(1.434, 190.3, 228.311, 5.210, 4321543),
    animals = c('dog', 'cat', 'camel', 'pig', 'mouse'),
    factors = factor(c('rain', 'grain', 'stain', 'pain', 'main')),
    ints    = c(10L, 20L, 40L, 30L, 50L),
    stringsAsFactors = FALSE
  )

  expect_silent(st1 <- mash_table(tdat1, tdat1, tdat1, tdat1, rem_ext = '_xt'))
  of <- file.path(outdir, 'mash_table.xlsx')
  save_xlsx(st1, of, overwrite = TRUE)
  # hammr::excel(of)

  save_xlsx(st1, of, overwrite = TRUE, mash_method = 'col')
  # hammr::excel(of)


  of2 <- file.path(test_path(), 'test_out', 'mash_table_meta.xlsx')
  st1_meta <- meta_table(
    st1,
    tt_meta(
      table_id = 'tid',
      title = 'title',
      longtitle = 'longitle',
      subtitle = 'subtitle',
      footer = ' ---------------- ')
  )
  save_xlsx(st1_meta, of2, overwrite = TRUE)
  # hammr::excel(of2)
})


test_that("xlsx output for comp_tables works", {
  #* @testing save_xlsx.Comp_table
  #* @testing write_worksheet.Comp_table
  #* @testing as_workbook.Comp_table

  # Generate test data
  tdat <- list()
  for(i in seq_len(3)){
    tdat[[i]] <- data.frame(
      id = 1:6,
      small = letters[i:(i+5)],
      tall = LETTERS[i:(i+5)]
    )
  }

  expect_silent(tres <- comp_table_list(
    tdat,
    multinames = c('tab1', 'tab2', 'tab3')
  ))

  pub_tres <- meta_table(
    tres,
    tt_meta(
      't1',
      'comp pub tab',
      'a composite pub table',
      'for a testing purpose'
    ))

  # Ursula wants to export her comp_table as .xlsx
  # (the xlsx files have to be checked manually)
  td <- tempdir()
  wb <- as_workbook(tres)
  outfile <- file.path(td, 'comp_table.xlsx')

  expect_silent(openxlsx::saveWorkbook(wb, outfile, overwrite = TRUE))
  pub_wb    <- as_workbook(pub_tres)
  outfile <- file.path(td, 'pub_comp_table.xlsx')
  expect_silent(openxlsx::saveWorkbook(pub_wb, outfile, overwrite = TRUE))
  # hammr::excel(outfile)


  # When exporting comp_tables created with "by", the super-headings should
  # not cover the id_vars
  expect_silent(tres <- comp_table_list(
    tdat,
    multinames = c('tab1', 'tab2', 'tab3'),
    by = 'id'
  ))
  wb <- as_workbook(tres)

  outfile <- file.path(td, 'pub_comp_table_idvars.xlsx')
  expect_silent(openxlsx::saveWorkbook(wb, outfile, overwrite = TRUE))
  # hammr::excel(outfile)


  outfile <- file.path(td, 'pub_comp_table_meta.xlsx')
  save_xlsx(pub_tres, outfile)
  #hammr::excel(outfile)
})



test_that("xlsx output for stack_tables works", {
  #* @testing save_xlsx.Stack_table
  #* @testing write_worksheet.Stack_table
  #* @testing as_workbook.Stack_table
  #* @testing write_worksheet.Meta_table

  # Generate test data
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
  expect_silent(tmeta <- meta_table(tdat, meta = tm))


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
    multinames = c('tab1', 'tab2', 'tab3'),
    meta = tt_meta('t03', 'a comp table')
  )


  # Constructor works
  tdat <- stack_table(tmash, tcomp, tmeta, meta = tt_meta(
    table_id = 'rp1',
    title = 'stack table 1',
    longtitle = c('stack table 1 is a stack of tables', 'with a very long title', 'that spans several rows'),
    subtitle = 'with a subtitle',
    footer = c('that has a footer also', 'which goes over man lines')
  ))


  outfile <- file.path(outdir, 'stack_table.xlsx')
  wb <- as_workbook(tdat, insert_blank_row = FALSE)
  expect_silent(openxlsx::saveWorkbook(
    wb,
    outfile,
    overwrite = TRUE
  ))
  # openxlsx::openXL(outfile)
})


