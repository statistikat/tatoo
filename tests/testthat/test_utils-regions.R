context("utils-regions")


test_that("utils-regions works as expected", {
  tdat <- structure(
    c("header.Yxw5flkfj3zxW6ek", "table.qpbgjo3LJKy6mefv", "footer.AM81JEDnw38rJFqa"),
    sheet = c("1", "1", "1"),
    position = c("A1:A3",  "A5:J11", "A13:A13")
  )

  td <- attr(tdat, "position")

  excel_range_to_indices(td)


})


test_that("utils-regions works as expected", {
  tdat <- structure(
    c("header.Yxw5flkfj3zxW6ek", "table.qpbgjo3LJKy6mefv", "footer.AM81JEDnw38rJFqa"),
    sheet = c("1", "1", "1"),
    position = c("A1:A3",  "A5:J11", "A13:A13")
  )

  excel_regions_to_dt(tdat)
})

