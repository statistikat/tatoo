#' Stack tables
#'
#' Stack tables on top of each other. This can be used to print several tables
#' on one Excel sheet with \code{as_workbook} or \code{save_xlsx}.
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
#' @return a \code{Stacked_table}
#'
#' @aliases Stacked_table stacked_table
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
  res <- lapply(tables, ensure_valid_stack_table_classes)

  res <- Stacked_table(
    dat = res,
    spacing = spacing
  )

  if(!is.null(meta)){
    res <- tag_table(res, meta = meta)
  }

  return(res)
}




Stacked_table <- function(
  dat,
  spacing
){
  assert_that(is.list(dat))
  valid_classes <- c('Tatoo_table', 'data.table')
  assert_that(all(unlist(lapply(dat, hammr::is_any_class, valid_classes))))

  assert_that(purrr::is_scalar_numeric(spacing))
  assert_that(hammr::looks_like_integer(spacing))

  res <- data.table::copy(dat) %>%
    tatoo_table()

  data.table::setattr(res, 'class', union('Stacked_table', class(res)))
  data.table::setattr(res, 'spacing', as.integer(spacing))
  return(res)
}




#' Printing Stacked Tables
#'
#' @param dat A \code{Stacked_table}
#' @param ... passed on to \code{\link{print}}
#'
#' @return \code{dat} (invisibly)
#'
#' @export
print.Stacked_table <- function(dat, ...){
  print_several_tables(
    dat,
    indent = ' `  ',
    sep1 = '`',
    sep2 = '_',
    ...
  )

  invisible(dat)
}



# Utils -------------------------------------------------------------------
ensure_valid_stack_table_classes <- function(x){
  if(hammr::is_any_class(
    x,
    c('Tagged_table', 'Composite_table', 'Mashed_table', 'data.table'))
  ){
    return(x)
  } else {
    return(data.table::as.data.table(x))
  }
}
