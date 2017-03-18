#' Tatoo Table
#'
#' Constructor for the superclass of all `tatoo` table classes. Does
#' nothing by itself but is used by a few validity checks in this package.
#' Currently the followin `Tatoo_table` subclasses exist:
#'
#' * \code{\link{Tagged_table}}
#' * \code{\link{Composite_table}}
#' * \code{\link{Mashed_table}}
#' * \code{\link{Stacked_table}}
#'
#'
#' @param dat an object of any of the classes listed above
#'
#' @aliases Tatoo_table
#' @return
#' @md
#' @examples
tatoo_table <- function(
  dat
){
  assert_that(hammr::is_any_class(dat, c('list', 'data.table')))

  res <- dat
  class(res) <- union('Tatoo_table', class(dat))

  return(res)
}
