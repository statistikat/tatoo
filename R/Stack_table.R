Stacked_table <- function(
  dat,
  spacing
){
  res <- data.table::copy(dat) %>%
    data.table::setattr('class', c('Stacked_table', 'list')) %>%
    data.table::setattr('spacing', spacing)

  return(res)
}
#' Stack tables
#'
#' side by side or on top of each others
#'
#' @param ... A list whos elements can of class
#'   \code{\link{Tagged_table}},
#'   \code{\link{Mashed_table}},
#'   \code{\link{Composite_table}},
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

#' @export
stack_table_list <- function(
  tables,
  spacing = 2L,
  meta = NULL
){
  ensure_data_table <- function(x){
    if(is_any_class(
      x,
      c('Tagged_table', 'Composite_table', 'Mashed_table', 'data.table'))
    ){
      return(x)
    } else {
      return(data.table::as.data.table(x))
    }
  }

  res <- lapply(tables, ensure_data_table)

  res <- Stacked_table(
    dat = res,
    spacing = spacing
  )

  if(!is.null(meta)){
    res <- tag_table(res, meta = meta)
  }

  return(res)
}




#' @export
print.Stacked_table <- function(dat){
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
