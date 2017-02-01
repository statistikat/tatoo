#' Composite Table
#'
#' @param tables
#' @param titles
#'
#' @return
#' @export
#'
#' @examples
comp_table <- function(tables, titles = names(tables)){
  tables %assert_class% 'list'
  assert_that(length(titles) %identical% length(tables))
  for(table in tables){
    table %assert_class% 'data.frame'
    assert_that(nrow(table)  %identical% nrow(tables[[1]]))
  }

  res          <- do.call(cbind, tables)
  table_titles <- cumsum(unlist(lapply(tables, ncol)))
  names(table_titles) <- titles

  class(res) <- union('Comp_table', class(res))
  attr(res, 'titles') <- table_titles

  return(res)
}


print.Comp_table <- function(dat){


  print(attr(dat, 'titles'))
  cat('\n')
  print.data.frame(dat)
}
