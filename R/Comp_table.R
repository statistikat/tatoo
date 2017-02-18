#' Composite Table
#'
#' @param ...
#' @param titles
#'
#' @return
#' @export
#'
#' @examples
comp_table <- function(
  ...,
  titles,
  by = NULL,
  meta = NULL
){
  force(titles)

  tables <- list(...)
  comp_table_list(
    list(...),
    titles = titles,
    by = by,
    meta = meta
  )
}




#' Composite Table
#'
#' @param tables
#' @param titles
#'
#' @return
#' @export
#'
#' @examples
comp_table_list <- function(
  tables,
  titles = names(tables),
  by = NULL,
  meta = NULL
){
  # Pre-conditions
    assert_that(is.list(tables))
    for(table in tables){
      table %assert_class% 'data.frame'
      assert_that(nrow(table)  %identical% nrow(tables[[1]]))
    }

    if(!length(titles) %identical% length(tables)){
      stop(strwrap(
          'Titles must be specified, otherwise comp_table
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
    table_titles <- vector('integer', length(tables))
    for(i in seq_along(table_titles)){
      table_titles[[i]] <- ncol(tables[[i]]) - length(by)
    }

    if(length(by) > 0){
      table_titles <- c(length(by), table_titles)
      titles       <- c('', titles)
    }

    table_titles <- cumsum(table_titles)
    names(table_titles) <- titles


  # post conditions
    assert_that(max(table_titles) %identical% ncol(res))

    if(length(by) %identical% 0L){
      assert_that(min(table_titles) %identical% ncol(tables[[1]]))
    } else {
      assert_that(min(table_titles) %identical% length(by))
    }

    assert_that(table_titles %identical% sort(table_titles))


  # Return
    class(res) <- union('Comp_table', class(res))
    attr(res, 'titles') <- table_titles

    if(!is.null(meta)){
      res <- meta_table(res, meta = meta)
    }

    return(res)
}




print.Comp_table <- function(dat, row.names = FALSE, ...){
  assert_that(has_attr(dat, 'titles'))

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
    titles <- attr(dat, 'titles')
    res <- vector('list', length(titles))
    names(res) <- names(titles)

    for(i in seq_along(titles)){
      res[[i]] <- titles[(i-1):i]

      if(!identical(i, 1L)){
        sel_cols <- titles[i-1]:titles[i]
      } else {
        sel_cols <- 1:titles[[i]]
      }

      res[[i]] <- do.call(paste, c(dd[sel_cols], sep="   "))
    }

    tmp <- list()

    for(i in seq_along(res)){
      title  <- stringi::stri_pad_both(names(titles)[[i]], max(nchar(res[[i]])), '.')
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
