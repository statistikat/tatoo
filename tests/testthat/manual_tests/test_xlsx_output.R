context('Saving xlsx files')

test_that('save_xlsx saves files to disc for manual check', {
  # Setup
    source(rprojroot::find_testthat_root_file('testdata', 'testdata.R'))
    outdir <- rprojroot::find_testthat_root_file("testout")

  # Try saving a report containing mash tables
    of <- file.path(outdir, 'report_mash.xlsx')
    expect_silent(save_xlsx(t_report_mash, of, overwrite = TRUE))
    expect_true(file.exists(of))

    of <- file.path(outdir, 'report_comp.xlsx')
    expect_silent(save_xlsx(t_report_comp, of, overwrite = TRUE))
    expect_true(file.exists(of))

    of <- file.path(outdir, 'report_stack.xlsx')
    expect_silent(save_xlsx(t_report_stack, of, overwrite = TRUE))
    expect_true(file.exists(of))
})


test_that('save_tex mash_tablesk', {
  # Setup
  source(rprojroot::find_testthat_root_file('testdata', 'testdata.R'))
  outdir <- rprojroot::find_testthat_root_file("testout")

  of <- file.path(outdir, 'mash_table_blank.pdf')
  save_pdf(t_mash_1, of, overwrite = TRUE, keep_source = TRUE, insert_blank_row = TRUE)
  expect_true(file.exists(of))

  of <- file.path(outdir, 'mash_table.pdf')
  save_pdf(t_mash_1, of, overwrite = TRUE, keep_source = TRUE, insert_blank_row = FALSE)
  expect_true(file.exists(of))

  of <- file.path(outdir, 'mash_table.xlsx')
  save_xlsx(t_mash_1, of, overwrite = TRUE)
  expect_true(file.exists(of))



})
