context('Saving xlsx files')

test_that('save_xlsx saves files to disc for manual check', {
  # Setup
    source(file.path('..', 'testdata', 'testdata.R'))
    outdir <- file.path('..', 'testout')

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
