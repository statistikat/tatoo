#' Tatoo Table
#'
#' `Tatto_table` is the superclass of all the `*_table` classes made available
#' by this package. Each `Tatoo_table` provides a different way of combining
#' several tables (data.frames) into a single table. Those tables can then
#' be exported via [as_workbook()]/[save_xlsx()]. In the future, support for
#' latex and html export is also planned.
#'
#' Currently, the following subclasses exists:
#' * [Tagged_table]
#' * [Composite_table]
#' * [Mashed_table]
#' * [Stacked_table]
#'
#' The `tatoo_table()` function is just a constructor used internally and you
#' will not need to use it except if your planning on extending this package
#' with your own code.
#'
#' @param dat an object of any of the classes listed in the description
#' @family Tatoo tables
#' @aliases Tatoo_table
#' @md
tatoo_table <- function(
  dat
){
  assert_that(is_any_class(dat, c('list', 'data.table')))

  res <- dat
  class(res) <- union('Tatoo_table', class(dat))

  return(res)
}




#' @rdname tatoo_table
#' @export
is_Tatoo_table <- function(dat){
  inherits(dat, 'Tatoo_table')
}
