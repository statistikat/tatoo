context("write_worksheet")


test_that("write_worksheet works as expected", {
  tdat <- 'horrible[ ] * / \ ? : []*/\ ?: namenamenamenamenamenamenamename'
  expect_identical(
    sanitize_excel_sheet_names(tdat),
    "horrible_ _ _ _  _ _ ____ __ na"
  )
})
