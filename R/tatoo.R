#' Table tools: tools for combining data frames and exporting them to xlsx
#'
#' Tatoo ("Table Tools") provides three straight-forward helper to combine multiple
#' `data.frames` to tables that are otherwise awkward to create in base-r, and a
#' way to add metadata (id, title, ...) to a table. In addition, the Tatto_report
#' class is provided as a convenient helper to write several Tatoo tables to a
#' workbook, one table per worksheet.
#'
#' Tatoo tables and reports can directly be saved to .xlsx files, or convert to
#' `Workbook` objects with `as_workbook()` so that you can process them further
#' using the `openxlsx` package. While tatoo implements convenient print
#' methods so that you can preview the tables you created in the console, most of
#' the functionality provided by this package only makes real sense for .xlsx
#' export.
#'
#' @section Functions:
#' * [tag_table()]: add captioning (title, footer, ...) to a table
#' * [comp_table()]: like [cbind()] or [merge()], but retain multi-column headings
#' * [mash_table()]: combine data.frames so that their rows or columns alternate
#' * [stack_table()]: create a list of tables that can be exported to xlsx,
#'     all tables on the same worksheet on top of each others
#' * [compile_report()]: create a list of tables that can be exported to xlsx,
#'     one table per worksheet (a Stacked_table also counts as one table)
#' * [as_workbook()] / [save_xlsx()]: export to excel
#'
#'
#' @md
#' @import assertthat
#' @importFrom magrittr %>%
#' @importFrom purrr %||%
#' @importFrom data.table data.table as.data.table
#' @export as.data.table
#'
#' @docType package
#' @name tatoo
"_PACKAGE"

