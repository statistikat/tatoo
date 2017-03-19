# Ctors -------------------------------------------------------------------

#' Compose Tables
#'
#' @param ... \code{comp_table} only: individual `data.frames`. A name must be
#'   provided for each `data.frame` (see examples).
#' @param id_vars If \code{id_vars} is specified, the tables will be combined
#'   using \code{\link{merge}} on the columns specified in id_vars, otherwise
#'   the tables will be combined with \code{\link{cbind}}.
#' @param meta a \code{\link{TT_meta}} object. If speciefied, the resulting
#'   \code{comp_table} will be wrapped in a \code{\link{tag_table}}.
#'
#' @return a \code{Composite_table}
#'
#' @aliases Composite_table composite_table
#' @family Tatto tables
#' @export
comp_table <- function(
  ...,
  id_vars = NULL,
  meta = NULL
){
  dots <- list(...)

  is_named_list <- identical(length(names(dots)), length(dots))
  assert_that(is_named_list)

  comp_table_list(
    tables = dots,
    id_vars = id_vars,
    meta = meta
  )
}




#' @param tables \code{comp_table_list} only: A named list of data.frames with
#'   the same number of rows
#'
#' @rdname comp_table
#' @export
comp_table_list <- function(
  tables,
  id_vars = NULL,
  meta = NULL
){
  # Pre-conditions
  assert_that(is.list(tables))

  for(table in tables){
    assert_that(is.data.frame(table))
    assert_that(nrow(table) %identical% nrow(tables[[1]]))
  }

  if(!length(names(tables)) %identical% length(tables)){
    stop(hammr::str_nobreak(
      'names(tables) must be specified, otherwise comp_table
      would just be a wrapper for cbind.'))
  }

  # Combine the tables
  if(is.null(id_vars)){
    res          <- dplyr::bind_cols(tables)
  } else {
    id_vars_in_colnames <- tables %>%
      purrr::map(names) %>%
      purrr::map_lgl(function (x) id_vars %in% x)

    assert_that(all(id_vars_in_colnames))

    merger <- function(x, y)  {suppressWarnings(
      merge.data.frame(
        x,
        y,
        by = id_vars,
        all = TRUE,
        suffixes = c('', ''),
        sort = FALSE)
    )}
    res <- Reduce(merger, tables)
  }

  # Generate table-title cell positions (for xlsx / latex export).
  # if a "id_vars" was specified, this has to be considered when creating the indices
  table_multinames <- vector('integer', length(tables))
  for(i in seq_along(table_multinames)){
    table_multinames[[i]] <- ncol(tables[[i]]) - length(id_vars)
  }

  if(length(id_vars) > 0){
    table_multinames <- c(length(id_vars), table_multinames)
    table_names <- c('', names(tables))
  } else {
    table_names <- names(tables)
  }

  table_multinames <- cumsum(table_multinames)
  names(table_multinames) <- table_names


  # post conditions
  assert_that(max(table_multinames) %identical% ncol(res))

  if(length(id_vars) %identical% 0L){
    assert_that(min(table_multinames) %identical% ncol(tables[[1]]))
  } else {
    assert_that(min(table_multinames) %identical% length(id_vars))
  }

  assert_that(table_multinames %identical% sort(table_multinames))


  # Return
  res <- Composite_table(
    res,
    multinames = table_multinames
  )


  if(!is.null(meta)){
    res <- tag_table(res, meta = meta)
  }

  return(res)
}




Composite_table <- function(
  dat,
  multinames
){
  assert_that(is.data.frame(dat))
  assert_that(is.numeric(multinames))
  assert_that(all(hammr::looks_like_integer(multinames)))

  multinames <- structure(
    as.integer(multinames),
    names = names(multinames)
  )

  res <- data.table::copy(dat) %>%
    data.table::as.data.table() %>%
    tatoo_table()

  data.table::setattr(res, 'class', union('Composite_table', class(res)))
  data.table::setattr(res, 'multinames', multinames)

  assert_that(identical(
    class(res),
    c('Composite_table', 'Tatoo_table', 'data.table', 'data.frame'))
  )

  return(res)
}




# Methods -----------------------------------------------------------------

#' Printing Composite Tables
#'
#' @param dat A \code{Tagged_table}
#' @param right \code{logical}, indicating whether or not strings should be
#'   right-aligned. The default is left-alignment (the opposite of the
#'   standard \code{print.data.frame} method.
#' @param ... passed on to \code{\link{print}}
#'
#' @return \code{dat} (invisibly)
#'
#' @export
print.Composite_table <- function(
  dat,
  right = FALSE,
  ...
){
  assert_that(has_attr(dat, 'multinames'))

  # Pad columns
    prep_col <- function(x, colname){
      i_nan <- is.nan(x)
      i_na  <- is.na(x)
      x <- as.character(x)
      x[i_nan] <- 'NAN'
      x[i_na]  <- 'NA'
      x <- c(colname, x)

      pad_width <- max(nchar(x))
      stringi::stri_pad_left(as.character(x), pad_width)
    }

    dd <- vector('list', ncol(dat))

    for(i in seq_along(dd)){
      dd[[i]] <- prep_col(dat[[i]], names(dat)[[i]])
    }


  # Merge columns that belong to the same title
    multinames <- attr(dat, 'multinames')
    res <- vector('list', length(multinames))
    names(res) <- names(multinames)

    for(i in seq_along(multinames)){
      res[[i]] <- multinames[(i-1):i]

      if(identical(i, 1L)){
        sel_cols <- 1:multinames[[i]]
      } else {
        sel_cols <- (multinames[i-1]+1):multinames[i]
      }

      res[[i]] <- do.call(paste, c(dd[sel_cols], sep="   "))
    }

    tmp <- list()

    for(i in seq_along(res)){
      title  <- stringi::stri_pad_both(names(multinames)[[i]], max(nchar(res[[i]])), '.')
      column <- stringi::stri_pad_both(res[[i]], nchar(title))
      sep    <- rep('  ', length(res[[i]]))

      tmp[[i]] <- list(column, sep)
      names(tmp[[i]]) <- c(title, '   ')
    }

    res2 <- unlist(tmp, recursive = FALSE)
    res2 <- as.data.frame(res2, fix.empty.names = FALSE, optional = TRUE)

    print(res2, right = right, ...)

    invisible(dat)
}



#' Convert a Composite Table to a plain data.table or data.frame
#'
#' As a \code{Composite_table} already is a \code{data.table} this function
#' does very little except stripping all additional attributes and classes,
#' as well as offering you the option to prepend the \code{multinames} before
#' the column names
#'
#' @param dat a \code{Composite_table}
#' @param multinames logical. Whether to prepend multinames before the column
#'   names
#' @param sep sepparator between multinames and individual column names
#' @param ... ignored
#'
#' @return a \code{data.table} or \code{data.frame}
#'
#' @export
as.data.table.Composite_table <- function(
  dat,
  multinames = TRUE,
  sep = '.',
  ...
){
  assert_that(is.flag(multinames))
  assert_that(purrr::is_scalar_character(sep))
  if(!multinames){
    return(data.table:::as.data.table.data.table(dat))
  } else {
    res <- data.table::copy(dat)
    multinames <- attr(res, 'multinames')
    name_idx <- 1

    for(i in seq_along(res)){
      names(res)[[i]] <- composite_name(
        names(multinames[name_idx]),
        names(res)[[i]],
        sep = sep
      )

      if(i == multinames[name_idx]){
        name_idx <- name_idx + 1
      }
    }

    return(data.table:::as.data.table.data.table(res))
  }
}




#' @rdname as.data.table.Composite_table
#' @export
as.data.frame.Composite_table <- function(
  dat,
  multinames = TRUE,
  sep = '.',
  ...
){
  as.data.frame(as.data.table.Composite_table(
    dat = dat,
    multinames = multinames,
    sep = sep))
}


# Setters -----------------------------------------------------------------

#' Set the multinames attribute of a Composite_table
#'
#' @param dat a Composite_table or data.frame
#' @param value a named character vector (see example)
#'
#' @export
`multinames<-` <- function(dat, value){
  assert_that(is.data.frame(dat))
  assert_that(max(value) == ncol(dat))

  if(is_class(dat, 'Composite_table')){
    dd  <- data.table::copy(dat)
    res <- data.table::setattr(dd, 'multinames', value)
  } else {
    res <- Composite_table(dat, multinames = value)
  }

  return(res)
}




# Utils -------------------------------------------------------------------

composite_name <- function(x, y, sep){
  if(x == ''){
    return(y)
  } else {
    paste(x, y, sep = sep)
  }
}




