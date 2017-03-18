Tagged_table <- function(
  dat,
  meta
){
  assert_that(hammr::is_any_class(
    dat,
    c('Tatoo_table', 'data.table')
  ))
  assert_that(is.list(meta))


  res <- data.table::copy(dat)
  if(!inherits(res, 'Tatoo_table')){
    res <- tatoo_table(res)
  }

  data.table::setattr(res, 'class', union('Tagged_table', class(res)))
  data.table::setattr(res, 'meta', meta)

  return(res)
}

#' Tag tables
#'
#' Add metadata/captioning (like table_id, title, footer) to a
#' \code{Tatoo_table} or \code{data.frame}. This metadata will be used by
#' \code{print} methods and export functions such as \code{\link{as_workbook}}
#' or \code{\link{save_xlsx}}.
#'
#' For convenience, functions to get and set the basic metadata elements
#' such as \code{title} are also provided. Those can be used to either
#' modify an existing \code{Tagged_table}, or convert an object that is not yet
#' a \code{Tagged_table} to one.
#'
#' @param dat A \code{Tatto_table} object or anything that can be coerced to a
#' \code{\link{data.table}} with \code{as.data.table}
#' @param meta a \code{\link{tt_meta}} object
#'
#' @return a \code{Tagged_table}
#'
#' @aliases Tagged_table tagged_table
#' @seealso tt_meta
#' @rdname tag_table
#' @export
#'
#' @examples
#'
#' dat <- data.frame(
#'   name  = c("hans", "franz", "dolores"),
#'   grade = c(1, 3, 2)
#' )
#'
#' table_metadata <- tt_meta(
#'   table_id = "Tab1",
#'   title = "Grades",
#'   longtitle = "grades of the final examination"
#' )
#'
#' # Metdata can be assign in a formal way or via set functions
#' dat <- tag_table(dat,  meta = table_metadata)
#' meta(dat) <- table_metadata
#'
#' # Table metadata is stored as an attribute, and cann be acces thus, or via
#' # more convientent get and set functions
#' attr(dat, 'meta')$title
#' title(dat)
#' longtitle(dat) <- "Grades of the final examination"
#'
#' # [1] "Grades"
#'
#' print(dat)
#'
#' # Tab1: Grades - Grades of the final examination
#' #
#' # name grade
#' # 1:    hans     1
#' # 2:   franz     3
#' # 3: dolores     2
#'
tag_table <- function(
  dat,
  meta
){
  assert_that(is_class(meta, 'TT_meta'))

  if (is_any_class(
    dat,
    c('Tagged_table', 'Stacked_table', 'Mashed_table', 'Composite_table'))
  ){
    res <- data.table::copy(dat)
  } else {
    res <- data.table::copy(data.table::as.data.table(dat))
  }

  res <- Tagged_table(res, meta)
  return(res)
}




#' Printing Tagged Tables
#'
#' @param dat A \code{Tagged_table}
#' @param ... passed on to \code{\link{print}}
#'
#' @return \code{dat} (invisibly)
#'
#' @export
print.Tagged_table <- function(dat, ...){
  dd    <- data.table::copy(dat)
  meta  <- attr(dd, 'meta')

  if(!is.null(meta)){
    cat(make_tag_table_print_title(meta), '\n\n')
  }

  NextMethod(print, dd, ...)

  if(!is.null(meta$footer)){
    footer <- paste(meta$footer, collapse = '\n')
    cat('\n', footer, '\n')
  }

  invisible(dat)
}




#' Tagged Table metadata
#'
#' Create a \code{TT_meta} (tagged table metadata) object. In the future,
#' different styling will be supported for title, longtitle and subtitle to
#' make the distinction more meaningfull.
#'
#' @param table_id A scalar (will be coerced to \code{character})
#' @param title A scalar (will be coerced to \code{character})
#' @param longtitle A vector. If \code{length > 1} the title will be displayed
#'   in several rows
#' @param subtitle A vector. If \code{length > 1} the title will be displayed
#'   in several rows
#' @param footer A vector. If \code{length > 1} the title will be displayed
#'   in several rows
#'
#' @return a \code{TT_meta} object.
#' @seealso tag_table
#' @rdname tt_meta
#'
#' @export
tt_meta <- function(
  table_id = NULL,
  title = NULL,
  longtitle = title,
  subtitle = NULL,
  footer = NULL
){
  assert_that(purrr::is_scalar_atomic(table_id) || is.null(table_id))
  assert_that(purrr::is_scalar_atomic(title) || is.null(title))

  assert_that(is.null(longtitle) ||purrr::is_atomic(longtitle))
  assert_that(is.null(subtitle) || purrr::is_atomic(subtitle))
  assert_that(is.null(footer) || purrr::is_atomic(footer))

  if(all(
      is.null(table_id),
      is.null(title),
      is.null(longtitle),
      is.null(subtitle),
      is.null(footer))
  ){
    stop(
      'Tagged_tables must at least contain one of the following:
      table_id, title, longtitle, subtitle or footer'
    )
  }



  res <- list(
    table_id   = table_id,
    title     = title,
    longtitle = longtitle,
    subtitle  = subtitle,
    footer = footer
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
make_tag_table_print_title <- function(meta, show_subtitle = TRUE){
  assert_that(is.flag(show_subtitle))
  meta %assert_class% 'TT_meta'

  table_id  <- meta$table_id
  title     <- meta$title %||% ''
  longtitle <- paste(meta$longtitle, collapse = '\n')

  if(!is.null(meta$subtitle)){
    subtitle <- paste(meta$subtitle, collapse = '\n')
  } else {
    subtitle <- NULL
  }

  res <- ''

  if(!is.null(table_id)){
    res <- table_id
  }

  if(!is.null(title)){
    if(nchar(res) > 0){
      res <- sprintf('%s: %s', table_id, title)
    } else {
      res <- title
    }
  }

  if(nchar(longtitle) > 0 && !identical(longtitle, title)){
    if(nchar(res) > 0){
      res <- sprintf('%s - %s', res, longtitle)
    } else {
      res <- longtitle
    }
  }

  if(show_subtitle && !is.null(subtitle)){
    res <- paste0(
      res, '\n', subtitle
    )
    assert_that(length(res) %identical% 1L)
  }

  return(res)
}




# Meta assignment functions -----------------------------------------------

#' @rdname tag_table
#' @export
`meta<-` <- function(dat, value){
  if(is.null(value)){
    res <- data.table::copy(dat)
    class(res) <- class(res)[class(res) != 'Tagged_table']
    attr(res, 'meta', NULL)

  } else{
    res <- tag_table(dat, value)
  }

  return(res)
}

#' @export
meta <- function(dat){
  attr(dat, 'meta')
}

#' @rdname tag_table
#' @export
`table_id<-` <- function(dat, value){
  ass <- list(table_id = value)
  assign_tt_meta(dat, ass)
}

#' @export
table_id <- function(dat){
  attr(dat, 'meta')$table_id
}

#' @rdname tag_table
#' @export
`title<-` <- function(dat, value){
  ass <- list(title = value)
  assign_tt_meta(dat, ass)
}

#' @export
title <- function(dat){
  attr(dat, 'meta')$title
}

#' @rdname tag_table
#' @export
`longtitle<-` <- function(dat, value){
  ass <- list(longtitle = value)
  assign_tt_meta(dat, ass)
}

#' @export
longtitle <- function(dat){
  attr(dat, 'meta')$longtitle
}

#' @rdname tag_table
#' @export
`subtitle<-` <- function(dat, value){
  ass <- list(subtitle = value)
  assign_tt_meta(dat, ass)
}

#' @export
subtitle <- function(dat){
  attr(dat, 'meta')$subtitle
}

#' @rdname tag_table
#' @export
`footer<-` <- function(dat, value){
  ass <- list(footer = value)
  assign_tt_meta(dat, ass)
}

#' @export
footer <- function(dat){
  attr(dat, 'meta')$footer
}



# Utils -------------------------------------------------------------------

#' Assign tt_metadata elements
#'
#' Internal function used by the metdata set functions
assign_tt_meta <- function(dat, assignment){
  assert_that(purrr::is_scalar_list(assignment))
  assert_that(identical(
    length(names(assignment)), 1L
  ))

  if(inherits(dat, 'Tagged_table')){
    res <- data.table::copy(dat)
    ass <- assignment[[1]]

    if(is.null(ass)){
      attr(res, 'meta')[names(assignment)] <- list(NULL)
    } else {
      attr(res, 'meta')[[names(assignment)]] <- ass
    }

  } else{
    res <- tag_table(
      dat,
      meta = do.call(tt_meta, assignment)
    )
  }

  return(res)
}
