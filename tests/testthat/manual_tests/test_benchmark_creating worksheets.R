library(microbenchmark)

source(file.path(rprojroot::find_testthat_root_file(), 'testdata', 'testdata.R'))



microbenchmark(
  as_workbook(t_report_stack),
  as_workbook(t_report_stack, named_regions = FALSE)
)


x <- as_workbook(t_report_stack)
y <- as_workbook(t_report_stack, named_regions = FALSE)

openxlsx::getNamedRegions(x)
openxlsx::getNamedRegions(y)
