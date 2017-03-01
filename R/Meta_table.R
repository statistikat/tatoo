#' Umbrella Class for Publication Tables
#'
#' The \code{meta_table} class does very little by itself except adding a
#' metadata attributes to a \code{data.frame} and providing a structure for how
#' you should procede when creating new reports for publication within the gv*
#' family of packages.
#'
#' A publication table should be a table that is nearly publication ready. It
#' should contain (only) the data desired in the final publication document, but
#' still have the original variable names and levels and accuracy (no rounding)
#' as in the analysis step. To achive the final publication ready table the
#' function \code{\link{polish}} should be used.
#'
#' In practice you will want to create a subclasses of \link{meta_table} for
#' for each distinct table in your report and implement your own polish methods
#' for them. Look at the source code of the \code{gvroad} package for examples.
#'
#' @param dat A \code{Meta_table}, \code{Mash_table}, \code{\link{Stack_table}},
#'   or anything that can be coerced to a \code{\link{data.table}} with
#'   \code{as.data.table}
#' @param meta a \code{\link{tt_meta}} object
#'
#' @return An object of class 'Meta_table'
#' @export
#'
#' @examples
#' # Simplified version of how creating and polishing a meta_table could look
#'
#' \dontrun{
#' dat <- data.frame(
#'   name  = c("hans", "franz", "dolores"),
#'   grade = c(1, 3, 2)
#' )
#'
#' dat <- meta_table(
#'   dat,
#'   tt_meta(
#'     "tab1",
#'     "grades",
#'     "Grades of the final examination")
#' )
#'
#' class(dat) <- c("Meta_tableTab1", class(dat))
#'
#' polish.Meta_tableTab1 <- function(dat){
#'  #...
#' }
#'
#' polish(dat)
#' }
meta_table <- function(dat, meta = NULL){
  assert_that(is.null(meta) || is_class(meta, 'TT_meta'))

  if (is_any_class(dat, c('Meta_table', 'Stack_table', 'Comp_table'))){
    dd <- data.table::copy(dat)
  } else {
    dd <- data.table::copy(data.table::as.data.table(dat))
  }

  if (!is.null(meta)){
    data.table::setattr(dat, 'meta', meta)
  }

  class(dat) <- union('Meta_table', class(dat))
  return(dat)
}




#' @export
print.Meta_table <- function(dat, ...){
  dd    <- data.table::copy(dat)
  meta  <- attr(dd, 'meta')

  if(!is.null(meta)){
    cat(make_meta_table_print_title(meta), '\n\n')
  }

  NextMethod(print, dd, ...)

  if(!is.null(meta$footer)){
    footer <- paste(meta$footer, collapse = '\n')
    cat('\n', footer, '\n')
  }
}





#' Pub Table Metadata
#'
#' Create a pub table metadata object
#'
#' @param table_id A vector of length 1
#' @param title A vector of length 1
#' @param longtitle A vector. If length >1 the title will be displayed in
#'   several rows
#' @param subtitle A vector
#' @param footer A vector
#' @param ... Additional arguments that will be contained in the final object.
#'   they are passed on to list. This is only usefull if you develop a package
#'   that wants to want to create subclasses of tt_meta.
#'
#' @export
tt_meta <- function(
  table_id,
  title,
  longtitle = title,
  subtitle = NULL,
  footer = NULL,
  ...
){
  assert_that(purrr::is_scalar_atomic(table_id))
  assert_that(purrr::is_scalar_atomic(title))
  assert_that(purrr::is_atomic(longtitle))

  assert_that(
    is.null(subtitle) | purrr::is_atomic(subtitle)
  )

  assert_that(
    is.null(footer) | purrr::is_atomic(footer)
  )

  res <- list(
    table_id   = table_id,
    title     = title,
    longtitle = longtitle,
    subtitle  = subtitle,
    footer = footer,
    ...
  )

  class(res) <- c('TT_meta', 'list')

  assert_that(is_valid(res))
  return(res)
}




#' @export
is_valid.TT_meta <- function(dat){
  res <- list()

  res$elements_are_scalars <- all(unlist(
    lapply(res, function(x) assertthat::is.scalar(x) || is.null(x))
  ))

  hammr::all_with_warning(res)
}




#' @export
make_meta_table_print_title <- function(meta, show_subtitle = TRUE){
  assert_that(is.flag(show_subtitle))
  meta %assert_class% 'TT_meta'

  table_id  <- meta$table_id
  title     <- paste(meta$title, collapse = '\n')
  longtitle <- paste(meta$longtitle, collapse = '\n')

  if(is.null(meta$subtitle)){
    subtitle <- NULL
  } else {
    subtitle  <- paste(meta$subtitle, collapse = '\n')
  }

  res <- table_id

  if(!table_id %identical% title){
    res <- sprintf('%s: %s', table_id, title)
    assert_that(length(res) %identical% 1L)
  }

  if(!longtitle %identical% title){
    res <- sprintf('%s - %s', res, longtitle)
    assert_that(length(res) %identical% 1L)
  }

  if(show_subtitle && !is.null(subtitle)){
    res <- paste0(
      res, '\n', subtitle
    )
    assert_that(length(res) %identical% 1L)
  }

  return(res)
}
