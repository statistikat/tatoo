#' Apply a function to all named regions on an openxlsx Workbook
#'
#' @param .wb
#' @param .pattern `character` scalar. A regex filter pattern for named region
#'   names (passed on to [grep()])
#' @param .fun A function with the formal arguments `wb`, `sheet` and either
#'   `rows`, `cols`, or both. For example:
#'   [openxlsx::addStyle()], [openxlsx::addFilter()],
#'   [openxlsx::setRowHeights()], [openxlsx::setColWidths()]
#' @param ... passed on to `.fun`
#'
#' @return
#' @export
#'
#' @examples
#'
#' x <- iris
#' title(iris) <- "Iris example table"
#' wb <- as_workbook(iris)
#'
#' # Display named regions in target workbook
#' named_regions(wb)
#'
#'
#' # Apply a style
#' # Keep in mind that openxlsx functions modify worksheets by reference.
#' # If you do not want this behaviour you can use map_named_regions instead.
#' style <- openxlsx::createStyle(textDecoration = "bold")
#' walk_named_regions(
#'   wb,
#'   .pattern = "header.*",
#'   .fun = openxlsx::addStyle,
#'   style = style
#' )
#'
#' openxlsx::openXL(wb)
#'
walk_named_regions <- function(
  .wb,
  .pattern = ".*",
  .fun,
  ...
){
  assert_that(inherits(.wb, "Workbook"))

  .formals <- names(formals(.fun))
  assert_that(
    all(c("wb", "sheet")  %in% .formals &&
    any(c("rows", "cols") %in% .formals
  )),
    msg = paste(
      ".fun must be a function with formal arguments 'wb', 'sheet', and either",
      "'rows', 'cols' or both. For example: 'openxlsx::addStyle()',",
      "'openxlsx::addFilter()', 'openxlsx::setRowHeights()'"
  ))

  rgs <- named_regions(.wb)
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




#' @rdname walk_named_regions
map_named_regions<- function(
  .wb,
  .pattern = ".*",
  .fun,
  ...
){
  wb <- openxlsx::copyWorkbook(.wb)
  walk_named_regions(.wb = wb, .pattern = .pattern, .fun = .fun, ...)
}
