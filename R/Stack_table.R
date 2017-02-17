#' Stack tables
#'
#' side by side or on top of each others
#'
#' @param ... A list whos elements can of class
#'   \code{\link{Meta_table}},
#'   \code{\link{Mash_table}},
#'   \code{\link{Comp_table}},
#'   or anything that can be coerced to a \code{data.frame} with
#'   \code{as.data.frame}
#'
#' @param meta a \code{\link{ttmeta}} object (optional)
#'
#' @return
#' @export
#'
#' @examples
stack_table <- function(
  ...,
  meta = NULL
){
  dl <- list(...)









  class(res) <- c('Stack_table', 'list')
  return(res)
}
