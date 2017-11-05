# Ctors -------------------------------------------------------------------

#' Compile Tables Into a Report
#'
#' Compiles tables into a `Tatoo_report`. A `Tatoo_report` is just
#' a simple list object, but with special `print`, `as_workbook`,
#' and `save_xlsx` methods. This makes it easy to save an arbitrary
#' number of tables to a single Excel workbook.
#'
#' @param dat for `compile_table_list`: A list of containing either
#'   `Tatoo_table` or `data.frame` objects.
#' @param ... for compile_table`: individual Tatoo_table` or
#'   data.frame` objects
#'
#' @return A `Tatoo_report`: A list whose elements are either `data.frames`
#'   or [`Tatoo_table`]s
#'
#' @rdname Tatoo_report
#' @aliases Tatoo_report tatoo_report
#' @export
compile_report <- function(...){
  compile_report_list(list(...))
}




#' @export
#' @rdname Tatoo_report
compile_report_list <- function(dat){
  res <- data.table::copy(dat)
  tatoo_report(res)
}




tatoo_report <- function(dat){
  res <- data.table::copy(dat)
  class(res) <- union('Tatoo_report', class(res))
  assert_valid(res)
  return(res)
}




# methods -----------------------------------------------------------------

#' Test if Object is a Tatoo_report
#'
#' @template any_r
#' @templateVar class Tatoo_report
#' @templateVar fun is_Tatoo_report()
#' @template is_class
#'
#' @export
is_Tatoo_report <- function(x){
  inherits(x, 'Tatoo_report')
}


#' @export
is_valid.Tatoo_report <- function(x){
  res <- list()

  is_valid_elclass <- function(x) {
    is_any_class(x, c('Tatoo_table', 'data.frame'))
  }

  res$class <- is.list(x)
  res$elclasses <- lapply(x, is_valid_elclass) %>%
    unlist() %>%
    all()

  all_with_warning(res)
}




#' Printing Tatoo Reports
#'
#' @param x A \code{Tatoo_report}
#' @param ... passed on to \code{\link{print}}
#'
#' @return \code{x} (invisibly)
#'
#' @export
print.Tatoo_report <- function(x, ...){
  print_lines(as_lines(x))
  invisible(x)
}
