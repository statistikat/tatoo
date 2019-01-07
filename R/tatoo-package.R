#' tatoo: Combine and Export Data Frames
#'
#' @section Functions:
#'
#' * [tag_table()]: add captioning (title, footer, ...) to a table
#' * [comp_table()]: like [cbind()] or [merge()], but retain multi-column
#'   headings
#' * [mash_table()]: combine data.frames so that their
#'   rows or columns alternate. Mash tables are stored as lists that can be
#'   converted to data.tables, or you can use [rmash()] and [cmash()] to create
#'   `data.frames` directly.
#' * [stack_table()]: create a list of tables that can be exported to xlsx, all
#'   tables on the same worksheet on top of each others
#' * [compile_report()]: create a list of tables that can be exported
#'   to xlsx, one table per worksheet (a Stacked_table also counts as one table)
#' * [as_workbook()] / [save_xlsx()]: To export any of the objects described
#'   above to excel workbooks.
#'
#'
#' @import assertthat
#' @importFrom magrittr %>%
#' @importFrom data.table data.table as.data.table
#'
#' @docType package
#' @name tatoo
"_PACKAGE"




if(getRversion() >= "2.15.1")  utils::globalVariables(c(".", "sheet"))




ignore_import_warnings <- function(...){
  colt::clt_bg_accent("blubb")
}
