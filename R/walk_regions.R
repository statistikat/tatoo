#' Apply a function to all named regions on an openxlsx Workbook
#'
#' This applies a `.fun` to all named regions in a workbook names match
#' `.pattern`. This is especially useful since [as_workbook()] methods for
#' `Tatoo_tables` add named regions for certain parts of the Table. See also
#' `vignette("named_regions")` for how the names of named regions are
#' constructed by tatoo.
#'
#'
#' @param .wb an openxlsx `Workbook` Object
#' @param .pattern `character` scalar. A regex filter pattern for named region
#'   names (passed on to [grep()])
#' @param .fun A function with the formal arguments `wb`, `sheet` and either
#'   `rows`, `cols`, or both. For example:
#'   [openxlsx::addStyle()], [openxlsx::addFilter()],
#'   [openxlsx::setRowHeights()], [openxlsx::setColWidths()]
#' @param ... passed on to `.fun`
#'
#' @return `walk_regions` returns `.wb`. `map_regions` returns a modified copy
#'   of `.wb`
#' @export
#'
#' @examples
#'
#' x <- iris
#' title(iris) <- "Iris example table"
#' wb <- as_workbook(iris)
#'
#' regions(wb)  # display regions
#'
#'
#' # Apply a style
#' # Keep in mind that openxlsx functions modify worksheets by reference.
#' # If you do not want this behaviour you can use map_regions instead.
#'
#' style <- openxlsx::createStyle(textDecoration = "bold")
#' walk_regions(
#'   wb,
#'   .pattern = "colnames.*",
#'   .fun = openxlsx::addStyle,
#'   style = style
#' )
#'
#' \dontrun{
#'   openxlsx::openXL(wb)
#' }
#'
#'
#'
walk_regions <- function(
  .wb,
  .pattern = ".*",
  .fun,
  ...
){
  assert_that(inherits(.wb, "Workbook"))

  .formals <- names(formals(.fun))
  assert_that(
    all(c("wb", "sheet")  %in% .formals) &&
    any(c("rows", "cols") %in% .formals),
    msg = paste(
      ".fun must be a function with formal arguments 'wb', 'sheet', and either",
      "'rows', 'cols' or both. For example: 'openxlsx::addStyle()',",
      "'openxlsx::addFilter()', 'openxlsx::setRowHeights()'"
  ))

  rgs <- regions(.wb)
  rgs <- rgs[grep(.pattern, rgs$region)]

  for (s in rgs$sheet){
    rsub <- rgs[sheet == s]

    if (all(c("cols", "rows") %in% .formals)) {
      .fun(wb = .wb, sheet = s, rows = rsub$rows, cols = rsub$cols, ...)
    } else if ("cols" %in% .formals){
      .fun(wb = .wb, sheet = s, cols = rsub$cols, ...)
    } else if ("rows" %in% .formals){
      .fun(wb = .wb, sheet = s, rows = rsub$rows, ...)
    }
  }

  return(.wb)
}




#' `map_regions()` is a wrapper for `walk_regions()` for people who prefer
#' standard R copy-on-modify semantics over openxlsx's pass-by-reference.
#'
#' @rdname walk_regions
map_regions<- function(
  .wb,
  .pattern = ".*",
  .fun,
  ...
){
  wb <- openxlsx::copyWorkbook(.wb)
  walk_regions(.wb = wb, .pattern = .pattern, .fun = .fun, ...)
}
