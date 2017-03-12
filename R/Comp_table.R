#' Composite Table
#'
#' @param ... \code{comp_table} only:
#' @param multinames Titles of subtables (one for each element of
#'   \code{...} / \code{tables})
#' @param by If \code{by} is specified, the tables will be combined using
#'   \code{\link{merge}} on the columns specified in by, otherwise the tables
#'   will be combined with \code{\link{cbind}}.
#' @param meta a \code{\link{TT_meta}} object. If speciefied, the resulting
#'   \code{comp_table} will be wrapped in a \code{\link{meta_table}}.
#'
#' @return
#' @export
#'
#' @examples
comp_table <- function(
  ...,
  multinames,
  by = NULL,
  meta = NULL
){
  force(multinames) # fail early if argument was not provided

  comp_table_list(
    tables = list(...),
    multinames = multinames,
    by = by,
    meta = meta
  )
}




#' Composite Table
#'
#' @param tables \code{comp_table_list} only: A list of data.frames with the same number of rows
#'
#' @return
#' @rdname comp_table
#' @export
#'
#' @examples
comp_table_list <- function(
  tables,
  multinames = names(tables),
  by = NULL,
  meta = NULL
){
  # Pre-conditions
    assert_that(is.list(tables))
    for(table in tables){
      table %assert_class% 'data.frame'
      assert_that(nrow(table)  %identical% nrow(tables[[1]]))
    }

    if(!length(multinames) %identical% length(tables)){
      stop(strwrap(
          'multinames must be specified, otherwise comp_table
           would just be a wrapper for cbind.'))
    }


  # Combine the tables
    if(is.null(by)){
      res          <- dplyr::bind_cols(tables)
    } else {
      merger <- function(x, y)  {suppressWarnings(
          merge.data.frame(
            x,
            y,
            by = by,
            all = TRUE,
            suffixes = c('', ''),
            sort = FALSE)
        )}
      res <- Reduce(merger, tables)
    }


  # Generate table-title cell positions (for xlsx / latex export).
  # if a "by" was specified, this has to be considered when creating the indices
    table_multinames <- vector('integer', length(tables))
    for(i in seq_along(table_multinames)){
      table_multinames[[i]] <- ncol(tables[[i]]) - length(by)
    }

    if(length(by) > 0){
      table_multinames <- c(length(by), table_multinames)
      multinames       <- c('', multinames)
    }

    table_multinames <- cumsum(table_multinames)
    names(table_multinames) <- multinames


  # post conditions
    assert_that(max(table_multinames) %identical% ncol(res))

    if(length(by) %identical% 0L){
      assert_that(min(table_multinames) %identical% ncol(tables[[1]]))
    } else {
      assert_that(min(table_multinames) %identical% length(by))
    }

    assert_that(table_multinames %identical% sort(table_multinames))


  # Return
    res <- as.data.table(res)
    class(res) <- union('Comp_table', class(res))
    attr(res, 'multinames') <- table_multinames

    if(!is.null(meta)){
      res <- meta_table(res, meta = meta)
    }

    return(res)
}



#' @export
print.Comp_table <- function(dat, row.names = FALSE, ...){
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

    print(res2, row.names = FALSE, right = FALSE)

    invisible(dat)
}



#' @export
as.data.table.Comp_table <- function(
  dat,
  multinames = TRUE,
  sep = '.'
){
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


composite_name <- function(x, y, sep){
  if(x == ''){
    return(y)
  } else {
    paste(x, y, sep = sep)
  }
}
