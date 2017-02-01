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
#' @param dat a \code{data.frame}
#' @param meta a \code{\link{pub_tableMeta}} object
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
#'   pub_tableMeta(
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
  dat <- data.table::copy(dat)
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
  cat(make_pub_table_print_title(meta), '\n\n')
  print(data.table::as.data.table(dd), ...)
}


#' Pub Table Metadata
#'
#' Create a pub table metadata object
#'
#' @param tableId
#'
#' @param title
#' @param longtitle
#' @param ...
#'
#' @export
pub_tableMeta <- function(
  tableId,
  title,
  longtitle = title,
  subtitle = NULL,
  ...
){
  tableId   %assert_class% 'character'
  title     %assert_class% 'character'
  longtitle %assert_class% 'character'
  assert_that(
    is.null(subtitle) | is.character(subtitle)
  )

  res <- list(
    tableId   = tableId,
    title     = title,
    longtitle = longtitle,
    subtitle  = subtitle,
    ...
  )

  class(res) <- c('Pub_tableMeta', 'list')
  return(res)
}



#' @export
pub_tableMaker <- function(fun, idVars){
  fun %assert_class% 'function'
  assert_that(is.character(idVars))

  class(fun) <- c('Pub_tableMaker', 'function')
  data.table::setattr(fun, 'idVars', idVars)
  return(fun)
}



#' @export
make_pub_table_print_title <- function(meta, subtitle = TRUE){
  assert_that(is.flag(subtitle))
  meta %assert_class% 'Pub_tableMeta'

  title <- meta$tableId

  if(!meta$tableId %identical% meta$title){
    title <- sprintf('%s: %s', meta$tableId, meta$title)
  }

  if(!meta$longtitle %identical% meta$title){
    title <- sprintf('%s - %s', title, meta$longtitle)
  }

  if(subtitle && !is.null(meta$subtitle)){
    title <- paste0(
      title, '\n', meta$subtitle )
  }

  return(title)
}
