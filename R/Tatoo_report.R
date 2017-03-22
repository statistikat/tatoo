# Ctors -------------------------------------------------------------------

#' Compile tables into a report
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
#' @return A `Tatoo_report`: A list whose elements are either `data.frame`s
#'   or `\link{Tatoo_table}`s
#'
#' @md
#' @rdname Tatoo_report
#' @aliases Tatoo_report tatoo_report compile_report
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
  hammr::assert_valid(res)
  return(res)
}




# methods -----------------------------------------------------------------

#' @rdname Tatoo_report
#' @export
is_Tatoo_report <- function(dat, ...){
  inherits(dat, 'Tatoo_report')
}


#' @export
is_valid.Tatoo_report <- function(dat){
  res <- list()

  is_valid_elclass <- function(x) {
    hammr::is_any_class(x, c('Tatoo_table', 'data.frame'))
  }

  res$class <- is.list(dat)
  res$elclasses <- lapply(dat, is_valid_elclass) %>%
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

  make_table_heading <- function(x) {
    if ('Tagged_table' %in% class(x)){
      paste(class(x)[1:2], collapse = '> <')
    } else {
      class(x)[[1]]
    }
  }

  classes <- dat %>%
    lapply(make_table_heading) %>%
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
