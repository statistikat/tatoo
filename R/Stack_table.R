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
#' @param meta a \code{\link{tt_meta}} object (optional)
#'
#' @return
#' @export
#'
#' @examples
stack_table <- function(
  ...,
  spacing = 2L,
  meta = NULL
){
  stack_table_list(
    list(...),
    spacing = spacing,
    meta = meta
  )
}


stack_table_list <- function(
  tables,
  spacing = 2L,
  meta = NULL
){
  res <- tables
  attr(res, 'spacing') <- spacing
  class(res) <- c('Stack_table', 'list')
  if(!is.null(meta)){
    res <- meta_table(res, meta = meta)
  }

  if(!is.null(meta)){
    res <- meta_table(res, meta = meta)
  }

  return(res)
}


print.Stack_table <- function(dat){
  width <- getOption("width")-sqrt(getOption("width"))
  sep   <- paste(rep('#', width), collapse = '')
  sep2  <- paste(rep('_', width), collapse = '')

  cat(sep, '\n')
  for(i in seq_along(dat)){
    print(dat[[i]])
    if(i < length(dat)){
      cat(sep2, '\n')
    }
  }

  cat(sep)
}
