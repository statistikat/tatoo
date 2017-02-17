#' Umbrella Class for Publication Tables
#'
#' The \code{pub_table} class does very little by itself except adding a metadata
#' attributes to a \code{data.frame} and providing a structure for how you
#' should procede when creating new reports for publication within the
#' gv* family of packages.
#'
#' A publication table should be a table that is nearly publication ready. It
#' should contain (only) the data desired in the final publication document, but
#' still have the original variable names and levels and accuracy (no rounding)
#' as in the analysis step. To achive the final publication ready table the
#' function \code{\link{polish}} should be used.
#'
#' In practice you will want to create a subclasses of \link{pub_table} for
#' for each distinct table in your report and implement your own polish methods
#' for them. Look at the source code of the \code{gvroad} package for examples.
#'
#' @param dat A \code{Pub_table}, \code{Mash_table}, \code{\link{Stack_table}},
#'   or anything that can be coerced to a \code{\link{data.table}} with
#'   \code{as.data.table}
#' @param meta a \code{\link{pub_table_meta}} object
#'
#' @return An object of class 'Pub_table'
#' @export
#'
#' @examples
#' # Simplified version of how creating and polishing a pub_table could look
#'
#' \dontrun{
#' dat <- data.frame(
#'   name  = c("hans", "franz", "dolores"),
#'   grade = c(1, 3, 2)
#' )
#'
#' dat <- pub_table(
#'   dat,
#'   pub_table_meta(
#'     "tab1",
#'     "grades",
#'     "Grades of the final examination")
#' )
#'
#' class(dat) <- c("Pub_tableTab1", class(dat))
#'
#' polish.Pub_tableTab1 <- function(dat){
#'  #...
#' }
#'
#' polish(dat)
#' }
pub_table <- function(dat, meta = NULL){
  assert_that(is.null(meta) || is_class(meta, 'Pub_table_meta'))

  if (is_any_class(dat, c('Pub_table', 'Stack_table', 'Comp_table'))){
    dd <- data.table::copy(dat)
  } else {
    dd <- data.table::copy(data.table::as.data.table(dat))
  }

  if (!is.null(meta)){
    data.table::setattr(dat, 'meta', meta)
  }

  class(dat) <- union('Pub_table', class(dat))
  return(dat)
}



#' @export
print.Pub_table <- function(dat, ...){
  dd    <- data.table::copy(dat)
  meta  <- attr(dd, 'meta')

  if(!is.null(meta)){
    cat(make_pub_table_print_title(meta), '\n\n')
  }

  print(data.table::as.data.table(dd), ...)

  if(!is.null(meta$footer)){
    cat('\n', meta$footer, '\n')
  }
}




#' Pub Table Metadata
#'
#' Create a pub table metadata object
#'
#' @param table_id
#'
#' @param title
#' @param longtitle
#' @param ...
#'
#' @export
pub_table_meta <- function(
  table_id,
  title,
  longtitle = title,
  subtitle = NULL,
  footer = NULL,
  ...
){
  table_id   %assert_class% 'character'
  title     %assert_class% 'character'
  longtitle %assert_class% 'character'
  assert_that(
    is.null(subtitle) | is.character(subtitle)
  )

  res <- list(
    table_id   = table_id,
    title     = title,
    longtitle = longtitle,
    subtitle  = subtitle,
    footer = footer,
    ...
  )

  class(res) <- c('Pub_table_meta', 'list')

  assert_that(is_valid(res))
  return(res)
}



#' @export
is_valid.Pub_table_meta <- function(dat){
  res <- list()

  res$elements_are_scalars <- all(unlist(
    lapply(res, function(x) assertthat::is.scalar(x) || is.null(x))
  ))

  hammr::all_with_warning(res)
}



#' @export
pub_table_maker <- function(fun, idVars){
  fun %assert_class% 'function'
  assert_that(is.character(idVars))

  class(fun) <- c('Pub_table_maker', 'function')
  data.table::setattr(fun, 'idVars', idVars)
  return(fun)
}



#' @export
make_pub_table_print_title <- function(meta, subtitle = TRUE){
  assert_that(is.flag(subtitle))
  meta %assert_class% 'Pub_table_meta'

  title <- meta$table_id

  if(!meta$table_id %identical% meta$title){
    title <- sprintf('%s: %s', meta$table_id, meta$title)
    assert_that(length(title) %identical% 1L)
  }

  if(!meta$longtitle %identical% meta$title){
    title <- sprintf('%s - %s', title, meta$longtitle)
    assert_that(length(title) %identical% 1L)
  }

  if(subtitle && !is.null(meta$subtitle)){
    title <- paste0(
      title, '\n', meta$subtitle
    )
    assert_that(length(title) %identical% 1L)
  }

  return(title)
}
