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




#' @export
print.Stacked_table <- function(dat){
  tables_char <- lapply(dat, function(x) capture.output(print(x)))

  # Get width for print output
    tables_width <- tables_char %>%
      purrr::map(function(x) purrr::map_int(x, nchar)) %>%
      unlist() %>%
      max()
    assert_that(purrr::is_scalar_integer(output_width))


  # Define sepperators
    indent <- ' `  '
    sep    <- paste(rep('`', tables_width + nchar(indent) + 1), collapse = '')
    sep2   <- paste(rep('_', tables_width), collapse = '')



  cat('', sep, '\n')
  for(i in seq_along(tables)){
    lapply(tables[[i]], function(x) cat(indent, x, '\n'))
    if(i < length(tables)){
      cat(indent, sep2, '\n')
    }
  }
  cat(' `\n', sep, '\n')
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
