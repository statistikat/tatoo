context("Print methods")

#* @testing compile_report
#* @testing tatoo_report

test_that("Tatoo_report print method", {
#* @testing print.Tatoo_report

  source(file.path('..', 'test_data', 'test_data.R'))
  outfile <- file.path('..', 'test_out', 'test_print_methods.txt')

  res <- capture.output({
    cat('print(t_meta_simple)\n\n')
    print(t_meta_simple)

    cat('\n\n\n\nprint(t_mash_4)\n\n')
    print(t_mash_4)

    cat('\n\n\n\nprint(t_comp_1)\n\n')
    print(t_comp_1)

    cat('\n\n\n\nprint(t_stack_2)\n\n')
    print(t_stack_2)

    cat('\n\n\n\nprint(t_report)\n\n')
    print(t_report)
  })


  writeLines(res, outfile)
  expect_true(file.exists(outfile))
})
