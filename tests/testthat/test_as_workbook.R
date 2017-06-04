context("as_workbook")
#* @testing as_workbook
#* @testing as_workbook.default
#* @testing as_workbook.Tagged_table
#* @testing as_workbook.Mashed_table
#* @testing as_workbook.Composite_table
#* @testing as_workbook.Stacked_table
#* @testing as_workbook.Tatoo_report


test_that("as_workbook works as expected", {
  source(file.path(test_path(), 'testdata', 'testdata.R'))

  for(el in list(t_df1, t_mash_1, t_comp_1, t_stack_1))
  {
    # Check if workbook project is created without warnings or errors
    tres <- as_workbook(el)
    expect_is(tres, 'Workbook')

    # Check if workbook object contains the appropriate number of sheets
    if(!inherits(el, 'Tatoo_report')){
      expect_identical(length(names(tres)), 1L)
    } else {
      expect_identical(names(tres), names(el))
    }
  }

})
