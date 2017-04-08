context("Tatoo_report")

#* @testing compile_report
#* @testing tatoo_report

test_that("Tatoo_report print method", {
#* @testing print.Tatoo_report

  source(file.path(test_path(), 'test_data', 'test_data.R'))

  print(t_meta_simple)
  print(t_mash_4)
  print(t_comp_1)
  print(t_stack_2)

  print(t_report)

})
