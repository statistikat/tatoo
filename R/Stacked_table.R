# Constructors ------------------------------------------------------------

#' Stack Tables
#'
#' Stack tables on top of each other. This can be used to print several tables
#' on one Excel sheet with [as_workbook()] or [save_xlsx()].
#'
#' @param ... `stack_table()` only: Any number other [`Tatoo_table`] objects,
#'   or anything that can be coerced to a `data.frame`.
#' @param tables `stack_table_list()` only: Same as `(...)` for `stack_table`,
#'   just that a list can be supplied instead of individual arguments.
#' @param meta a [`tt_meta`] object (optional)
#' @param spacing Number of lineskips between the tables when exporting to
#'   xlsx
#'
#' @return A Stacked_table: a list of `Tatoo_tables` with additional
#'   `spacing` attribute that controls the default spacing between the tables
#'   when it is exported.
#'
#' @rdname Stacked_table
#' @aliases Stacked_table stacked_table stack_table
#' @seealso Attribute setter: [spacing<-]
#' @family Tatoo tables
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
  assert_that(is.scalar(spacing) && rlang::is_scalar_integerish(spacing))


  res <- data.table::copy(dat) %>%
    tatoo_table()

  data.table::setattr(res, 'class', union('Stacked_table', class(res)))
  data.table::setattr(res, 'spacing', as.integer(spacing))

  assert_valid(res)
  return(res)
}




#' @export
is_valid.Stacked_table <- function(x){
  res <- list(
    is_list     <- is.list(x),
    has_spacing <- rlang::is_scalar_integer(attr(x, 'spacing'))
  )
  all_with_warning(res)
}




# Methods -----------------------------------------------------------------


#' Test If Object is a Stacked_table
#'
#' @templateVar fun is_Stacked_table()
#' @templateVar class Stacked_table
#' @template any_r
#' @template is_class
#'
#'
#' @export
is_Stacked_table <- function(x){
  inherits(x, 'Stacked_table')
}




#' Printing Stacked Tables
#'
#' @param x A [`Stacked_table`]
#' @param ... passed on to [print()]
#'
#' @return `x` (invisibly)
#'
#' @export
print.Stacked_table <- function(x, ...){
  as_lines_several_tables(
    x,
    ...
  ) %>%
    print_lines()

  invisible(x)
}




# Setters -----------------------------------------------------------------

#' Set the spacing of a Stacked_table
#'
#' Set the number of lineskips between the tables when exporting to xlsx.
#'
#' @param x a `Stacked_table`
#' @param value a scalar integer
#'
#' @seealso [Stacked_table]
#' @export
`spacing<-` <- function(x, value){
  x %assert_class% 'Stacked_table'
  assert_that(rlang::is_scalar_integerish(value) && is.scalar(value))

  value <- as.integer(value)
  res <- data.table::copy(x)

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
