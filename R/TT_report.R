#' A list of Meta_tables
#'
#' A Pub report is a list of Meta_tables. Right now this class is mostly a
#' placeholder, but in the future it will support various export methods.
#'
#' @param dat
#'
#' @return An object of class 'TT_report'
#' @export
tt_report <- function(dat){
  res <- data.table::copy(dat)
  class(res) <- union('TT_report', class(res))
  hammr::assert_valid(res)
  return(res)
}




#' @export
is_valid.TT_report <- function(dat){
  res <- list()
  check_col_class <- function(x) 'Meta_table' %in% class(x)

  res$class     <- 'list' %in% class(dat)
  res$elclasses <- all(unlist(lapply(dat, check_col_class)))

  all_with_warning(res)
}




#' @export
print.TT_report <- function(
  dat,
  ...){

  for(el in dat){
    print(el, ...)
  }
}
