#' Compile tables into a report
#'
#' Compiles tables into a \code{Tatoo_report}. A \code{Tatoo_report} is just
#' a simple list object, but with special \code{print}, \code{as_workbook},
#' and \code{save_xlsx} methods. This makes it easy to save an arbitrary
#' number of tables to a single Excel workbook.
#'
#' @param dat for \code{compile_table_list}: A list of containing either
#'   \code{Tatoo_table} or \code{data.frame} objects.
#' @param ... for \code{compile_table}: individual \code{Tatoo_table} or
#'   \code{data.frame} objects
#'
#' @return An named \code{list} of class \code{Tatoo_report}
#' @rdname compile_report
#' @aliases Tatoo_report tatoo_report
#'
#' @export
compile_report <- function(...){
  compile_report_list(list(...))
}




#' @export
#' @rdname compile_report
compile_report_list <- function(dat){
  res <- data.table::copy(dat)
  tatoo_report(res)
}




tatoo_report <- function(dat){
  res <- data.table::copy(dat)
  class(res) <- union('Tatoo_report', class(res))
  hammr::assert_valid(res)
  return(res)
}




#' @export
is_valid.Tatoo_report <- function(dat){
  res <- list()

  is_valid_col_class <- function(x) {
    hammr::is_any_class(x, c('Tatoo_table', 'data.frame'))
  }

  res$class <- is.list(dat)
  res$elclasses <- lapply(dat, is_valid_col_class) %>%
    unlist() %>%
    all()

  all_with_warning(res)
}




#' Printing Tatoo Reports
#'
#' @param dat A \code{Tatoo_report}
#' @param ... passed on to \code{\link{print}}
#'
#' @return \code{dat} (invisibly)
#'
#' @export
print.Tatoo_report <- function(dat, ...){
  classes <- dat %>%
    lapply(function(x) class(x)[[1]]) %>%
    unlist() %>%
    sprintf('%s <%s> \n', names(dat) %||% '', .)

  print_several_tables(
    dat,
    indent = "::  ",
    sep1 = 0,
    sep2 = 2,
    headings = classes,
    ...
  )

  invisible(dat)
}
