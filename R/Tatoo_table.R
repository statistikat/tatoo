#' Tatoo Table
#'
#' Superclass for all tatoo table objects
#'
#' @param dat
#'
#' @return
#' @examples
tatoo_table <- function(
  dat
){
  assert_that(hammr::is_any_class(dat, c('list', 'data.table')))

  res <- dat
  class(res) <- union('Tatoo_table', class(dat))

  return(res)
}
