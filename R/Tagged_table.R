# Ctors - Tagged Table ----------------------------------------------------

#' Tag Tables
#'
#' Add metadata/captioning (like `table_id`, `title`, `footer`) to a
#' [`Tatoo_table`] or `data.frame`. This metadata will be used by
#' [print()] methods and export functions such as [`as_workbook()`]
#' or [`save_xlsx()`].
#'
#' @param dat A `Tatto_table` object or anything that can be coerced to a
#' [`data.table`].
#' @param meta a [`tt_meta`] object. Metadata can also be set and modified
#'   using setters (see [meta()])
#'
#' @return a `Tagged_table`: a `Tatoo_table` with an additional `meta`
#'   attribute
#'
#' @aliases Tagged_table tagged_table tag_table
#' @family Tatoo tables
#' @seealso Attribute setters: [meta<-()]
#' @seealso Tagged Table Metadata: [tt_meta()]
#' @rdname Tagged_table
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
#' # Metadata can be assign in a formal way or via set functions
#' dat <- tag_table(dat,  meta = table_metadata)
#' meta(dat) <- table_metadata
#'
#' # Table metadata is stored as an attribute, and cann be acces thus. It can
#' # also be modified via convenient set functions
#' attr(dat, 'meta')$title
#' meta(dat)$title
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




Tagged_table <- function(
  dat,
  meta
){
  assert_that(is_any_class(
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




# Ctors - TT_meta (Tagged Table Metadata) ---------------------------------

#' Tagged Table Metadata
#'
#' Create a `TT_meta` (tagged table metadata) object. In the future,
#' different styling will be supported for title, longtitle and subtitle to
#' make the distinction more meaningful.
#'
#' @param table_id A scalar (will be coerced to `character`)
#' @param title A scalar (will be coerced to `character`)
#' @param longtitle A vector. If `length > 1` the title will be displayed in
#'   several rows
#' @param subtitle A vector. If `length > 1` the title will be displayed in
#'   several rows
#' @param footer A vector. If `length > 1` the title will be displayed in
#'   several rows
#' @param .print_table_id `logical` vector. Whether or not `table_id` should be
#'   added to the title of the table in the various output formats. It is
#'   recommended to use table_ids only internally (i.e. for [walk_regions()]).
#'
#' @return a TT_meta object.
#' @seealso [Tagged_table]
#' @aliases TT_meta
#' @rdname tt_meta
#'
#' @export
tt_meta <- function(
  table_id = NULL,
  title = NULL,
  longtitle = title,
  subtitle = NULL,
  footer = NULL,
  .print_table_id = FALSE
){
  assert_that(
    is_scalar_atomic(table_id) || is.null(table_id),
    is_scalar_atomic(title) || is.null(title),

    is.null(longtitle) || is.atomic(longtitle),
    is.null(subtitle)  || is.atomic(subtitle),
    is.null(footer)    || is.atomic(footer),
    is_scalar_bool(.print_table_id)
  )

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




# Methods -----------------------------------------------------------------

#' Test If Object is a Tagged_table
#'
#' @template any_r
#'
#' @export
is_Tagged_table <- function(x){
  inherits(x, 'Tagged_table')
}




#' Printing Tagged Tables
#'
#' @param x a [Tagged_table]
#' @param ... passed on to [print()]
#'
#' @return `x` (invisibly)
#'
#' @export
print.Tagged_table <- function(x, ...){
  lines <- as_lines(x, ...)
  lines <- strip_newlines(lines)
  cat(lines, sep = "\n")

  invisible(x)
}




#' @export
is_valid.TT_meta <- function(x, ...){
  TRUE # not implemented
}




#' Printing Tagged Table Metadata
#'
#' @param x A [`TT_meta`] object
#' @param ... Ignored
#'
#' @return `x` (invisibly)
#'
#' @export
#'
print.TT_meta <- function(x, ...){
  name_width   <- max(unlist(lapply(names(x), crayon::col_nchar))) + 1
  print_string <- paste0('%', name_width, 's: %s\n')
  padded_newline <- rep(' ', name_width + 2) %>%
    paste(collapse = '')

  padded_newline <- paste0('\n', padded_newline)

  for(i in seq_along(x)){
    sprintf(
      print_string,
      names(x)[[i]], paste(x[[i]], collapse = padded_newline)
    ) %>%
      style_meta() %>%
      cat()
  }
  invisible(x)
}




# Setters -----------------------------------------------------------------

#' Set Tagged Table metadata
#'
#' Convenience functions to modify `Tagged_table` metadata. If `x` is not a
#' `Tagged_table` already, it will be converted to one.
#'
#' @param x a [`Tagged_table`] or any \R object that can be converted to one
#' @param value value to assign.
#'
#' @seealso [Tagged_table], [tt_meta]
#' @rdname tagged_set
#' @export
`meta<-` <- function(x, value){
  if(is.null(value)){
    res <- data.table::copy(x)
    class(res) <- class(res)[class(res) != 'Tagged_table']
    attr(res, 'meta', NULL)

  } else{
    res <- tag_table(x, value)
  }

  return(res)
}




#' @rdname tagged_set
#' @export
meta <- function(x){
  attr(x, 'meta')
}




#' @rdname tagged_set
#' @export
`table_id<-` <- function(x, value){
  ass <- list(table_id = value)
  assign_tt_meta(x, ass)
}




#' @rdname tagged_set
#' @export
table_id <- function(x){
  attr(x, 'meta')$table_id
}




#' @rdname tagged_set
#' @export
`title<-` <- function(x, value){
  ass <- list(title = value)
  assign_tt_meta(x, ass)
}




#' @rdname tagged_set
#' @export
`longtitle<-` <- function(x, value){
  ass <- list(longtitle = value)
  assign_tt_meta(x, ass)
}




#' @rdname tagged_set
#' @export
`subtitle<-` <- function(x, value){
  ass <- list(subtitle = value)
  assign_tt_meta(x, ass)
}




#' @rdname tagged_set
#' @export
`footer<-` <- function(x, value){
  ass <- list(footer = value)
  assign_tt_meta(x, ass)
}




# Utils -------------------------------------------------------------------

#' Assign tt_meta elements
#'
#' Internal function used by the metadata set functions
#'
#' @param x a [Tatoo_table] or data.frame
#' @param assignment A named list of length one, for example
#'   `list(longtitle = value)`
#'
assign_tt_meta <- function(x, assignment){
  assert_that(is_scalar_list(assignment))
  assert_that(identical(
    length(names(assignment)), 1L
  ))

  if(inherits(x, 'Tagged_table')){
    res <- data.table::copy(x)
    ass <- assignment[[1]]

    if(is.null(ass)){
      attr(res, 'meta')[names(assignment)] <- list(NULL)
    } else {
      attr(res, 'meta')[[names(assignment)]] <- ass
    }

  } else{
    res <- tag_table(
      x,
      meta = do.call(tt_meta, assignment)
    )
  }

  return(res)
}




make_tag_table_print_title <- function(meta, show_subtitle = TRUE){
  # Preconditions
    assert_that(is.flag(show_subtitle))
    meta %assert_class% 'TT_meta'


  # Process arguments
    sel <- lapply(meta, is.null) %>%
      unlist() %>%
      magrittr::not()

    titles <- meta[sel]

    if(titles$title %identical% titles$longtitle){
      titles$longtitle <- NULL
    }

    if(!show_subtitle){
      titles$subtitle <- NULL
    }


  # Logic
    titles <- lapply(titles, paste, collapse = '\n')

    if(!is.null(titles$title) && !is.null(titles$longtitle)){
      res <- paste(titles$title, titles$longtitle, sep = ' - ')

    } else if (!is.null(titles$title)){
      res <- titles$title

    } else if (!is.null(titles$longtitle)){
      res <- titles$longtitle

    } else {
      res <- NULL
    }


    if(!is.null(res) && !is.null(titles$table_id)){
      res <- paste(titles$table_id, res, sep = ': ')
    }


    if(!is.null(res) && !is.null(titles$subtitle)){
      res <- c(res, titles$subtitle)

    } else if (!is.null(titles$subtitle)) {
      res <- titles$subtitle

    } else if (is.null(res)) {
      res <- ""
    }


  unlist(strsplit(res, "\n", fixed = TRUE))
}
