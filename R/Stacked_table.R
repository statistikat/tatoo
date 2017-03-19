#' Stack tables
#'
#' Stack tables on top of each other. This can be used to print several tables
#' on one Excel sheet with `as_workbook` or `save_xlsx`.
#'
#' @param ... `stack_table()` only: Any number of objects of the following
#'   classes: `\link{Tagged_table}`, `\link{Mashed_table}`,
#'   `\link{Composite_table}`, or anything that can be coerced to a `data.frame`
#'   with `as.data.frame`
#'
#' @param tables `stack_table_list()` only: Same as `(...)` for `stack_table`,
#'   just that a list can be supplied instead of individual arguments.
#' @param meta a `\link{tt_meta}` object (optional)
#' @param spacing Number of lineskips between the tables when exporting
#'   stacked_tables
#'
#' @return a `Stacked_table`: a `list` of `Tatoo_table`s with additonal
#'   `spacing` attribute that controls the default spacing between the tables
#'   when it is exported.
#'
#' @md
#' @rdname Stacked_table
#' @aliases Stacked_table stacked_table stack_table
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




#' @rdname Stacked_table
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

  assert_valid(res)
  return(res)
}



is_valid.Stacked_table <- function(dat){
  res <- list(
    is_list     <- is.list(dat),
    has_spacing <- purrr::is_scalar_integer(attr(dat, 'spacing'))
  )
  all_with_warning(res)
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




#' @rdname Stacked_table
#' @export
`spacing<-` <- function(dat, value){
  dat %assert_class% 'Stacked_table'
  assert_that(hammr::looks_like_integer(value))

  value <- as.integer(value)
  res <- data.table::copy(dat)

  data.table::setattr(res, 'spacing', value)
  return(res)
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
