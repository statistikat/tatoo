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
  by = NULL
){
  force(titles)

  tables <- list(...)
  comp_table_list(
    list(...),
    titles = titles,
    by = by
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
  by = NULL
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
    return(res)
}




print.Comp_table <- function(dat){
  print(attr(dat, 'titles'))
  cat('\n')
  print.data.frame(dat)
}
