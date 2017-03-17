#' A list of Tagged_tables
#'
#' A Pub report is a list of Tagged_tables. Right now this class is mostly a
#' placeholder, but in the future it will support various export methods.
#'
#' @param dat
#'
#' @return An object of class 'Tatoo_report'
#' @export
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

  res$class     <- 'list' %in% class(dat)
  res$elclasses <- lapply(dat, is_valid_col_class) %>%
    unlist() %>%
    all()

  all_with_warning(res)
}




#' @export
print.Tatoo_report <- function(
  dat,
  ...){

  for(el in dat){
    print(el, ...)
  }
}
