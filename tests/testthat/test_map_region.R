context("map_region")


test_that("map_region works as expected", {
  source(file.path(test_path(), "testdata", "testdata.R"))

  .x <- as_workbook(t_report_stack)
  style <- openxlsx::createStyle(fgFill = "#FF0000")

  walk_named_regions(.x, .fun = openxlsx::addStyle, style = style)
  walk_named_regions(.x, .fun = openxlsx::setColWidths, widths = 2)
  walk_named_regions(.x, .fun = openxlsx::setRowHeights, heights = 32)


openxlsx::openXL(.x)

})
