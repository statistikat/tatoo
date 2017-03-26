# Constructors ------------------------------------------------------------

#' Stack tables
#'
#' Stack tables on top of each other. This can be used to print several tables
#' on one Excel sheet with [as_workbook()] or [save_xlsx()].
#'
#' @param ... `stack_table()` only: Any number other [Tatoo_table], or anything
#'   that can be coerced to a data.frame.
#' @param tables `stack_table_list()` only: Same as `(...)` for `stack_table`,
#'   just that a list can be supplied instead of individual arguments.
#' @param meta a `\link{tt_meta}` object (optional)
#' @param spacing Number of lineskips between the tables when exporting to
#'   xlsx
#'
#' @return a Stacked_table: a list of Tatoo_tables with additonal
#'   `spacing` attribute that controls the default spacing between the tables
#'   when it is exported.
#'
#' @md
#' @rdname Stacked_table
#' @aliases Stacked_table stacked_table stack_table
#' @family Tatto tables
#' @export
#'
#' @examples
#'
#' df1 <- iris[1:5, 3:5]
#' df2 <- iris[100:105, 3:5]
#'
#' stack_table(df1, df2)
#'
#' # ```````````````````````````````````````````
#' # `      Petal.Length Petal.Width Species
#' # `   1:          1.4         0.2  setosa
#' # `   2:          1.4         0.2  setosa
#' # `   3:          1.3         0.2  setosa
#' # `   4:          1.5         0.2  setosa
#' # `   5:          1.4         0.2  setosa
#' # `   ______________________________________
#' # `      Petal.Length Petal.Width    Species
#' # `   1:          4.1         1.3 versicolor
#' # `   2:          6.0         2.5  virginica
#' # `   3:          5.1         1.9  virginica
#' # `   4:          5.9         2.1  virginica
#' # `   5:          5.6         1.8  virginica
#' # `   6:          5.8         2.2  virginica
#' # `
#' # ```````````````````````````````````````````
#'
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
  assert_that(all(unlist(lapply(dat, is_any_class, valid_classes))))
  assert_that(purrr::is_scalar_numeric(spacing))
  assert_that(looks_like_integer(spacing))

  res <- data.table::copy(dat) %>%
    tatoo_table()

  data.table::setattr(res, 'class', union('Stacked_table', class(res)))
  data.table::setattr(res, 'spacing', as.integer(spacing))

  assert_valid(res)
  return(res)
}




#' @export
is_valid.Stacked_table <- function(dat){
  res <- list(
    is_list     <- is.list(dat),
    has_spacing <- purrr::is_scalar_integer(attr(dat, 'spacing'))
  )
  all_with_warning(res)
}





# Methods -----------------------------------------------------------------

#' @rdname Stacked_table
#' @export
is_Stacked_table <- function(dat, ...){
  inherits(dat, 'Stacked_table')
}




#' Printing Stacked Tables
#'
#' @param x A \code{Stacked_table}
#' @param ... passed on to \code{\link{print}}
#'
#' @return \code{x} (invisibly)
#'
#' @export
print.Stacked_table <- function(x, ...){
  print_several_tables(
    x,
    indent = ' `  ',
    sep1 = '`',
    sep2 = '_',
    ...
  )

  invisible(x)
}




# Setters -----------------------------------------------------------------

#' @rdname Stacked_table
#' @export
`spacing<-` <- function(dat, value){
  dat %assert_class% 'Stacked_table'
  assert_that(looks_like_integer(value))

  value <- as.integer(value)
  res <- data.table::copy(dat)

  data.table::setattr(res, 'spacing', value)
  return(res)
}




# Utils -------------------------------------------------------------------
ensure_valid_stack_table_classes <- function(x){
  if(is_any_class(
    x,
    c('Tagged_table', 'Composite_table', 'Mashed_table', 'data.table'))
  ){
    return(x)
  } else {
    return(data.table::as.data.table(x))
  }
}
