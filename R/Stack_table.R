#' Stack Table
#'
#' Stack tables are designed to make it easy to put together multidimensional
#' tables from two data.frames. An example where this might be useful is
#' if you have a data.frame of numeric values, and a second data.frame of
#' associated standard errors.
#'
#' Stack table provides a framework to stack those two data.frames together
#' into one data.frame with alternating rows or columns. You can then output the
#' stacked table as latex \code{\link{print_tex.Stack_table}}, as xlsx
#' \code{\link{save_as.Stack_table}} or simply as data.table or data.frame
#' via \code{as.data.table} or \code{as.data.frame}.
#'
#' If your goal is to present formated tables as latex or xlsx, you  should aslo
#' look into \code{\link{df_format}}, \code{\link{df_round}} and
#' \code{\link{df_signif}}.
#'
#' @param dat1
#' @param dat2
#' @param rem_ext
#'
#' @return
#' @export
#' @rdname stack_table
#'
#' @examples
stack_table <- function(dat1, dat2, rem_ext = NULL){
  dat1 %assert_class% 'data.frame'
  dat2 %assert_class% 'data.frame'
  assert_that(nrow(dat1)  %identical% nrow(dat2))
  assert_that(ncol(dat1)  %identical% ncol(dat2))

  dat1 <- as.data.table(data.table::copy(dat1))
  dat2 <- as.data.table(data.table::copy(dat2))

  if(!is.null(rem_ext)){
    data.table::setnames(dat1, gsub(rem_ext, '', names(dat1)))
    data.table::setnames(dat2, gsub(rem_ext, '', names(dat2)))
  }

  assert_that(identical(sort(names(dat1)), sort(names(dat2))))
  data.table::setcolorder(dat2, names(dat1))


  res <- list(dat1, dat2)
  class(res) <- c('Stack_table', 'list')
  return(res)
}


#' @export
#' @rdname stack_table
rstack <- function(dat1, dat2, rem_ext = NULL, ...){
  as.data.table(
    stack_table(dat1, dat2, rem_ext = rem_ext),
    stack_method = 'row',
    ...
  )
}


#' @export
#' @rdname stack_table
cstack <- function(dat1, dat2, rem_ext = NULL, ...){
  as.data.table(
    stack_table(dat1, dat2, rem_ext = rem_ext),
    stack_method = 'col',
    ...
  )
}


#' Title
#'
#' @param dat
#' @param stack_method
#' @param ... passed on to as.data.frame.data.table
#'
#' @return
#' @export
#'
#' @examples
as.data.table.Stack_table <- function(dat,
                                      stack_method = 'row',
                                      ...){

  stack_method  %assert_class% 'character'
  assert_that(is.scalar(stack))
  if(stack_method %in% c('c', 'col', 'column', 'columns')){
    res <- stack_cols(dat, ...)
  } else if(stack_method %in% c('r', 'row', 'rows')) {
    res <- stack_rows(dat, ...)
  } else{
    stop('stack_method must be either "row" or "col".')
  }

  return(as.data.table(res))
}


#' Title
#'
#' Stacking uses \code{data.table}s internally,
#'
#' @param dat
#' @param stack_method
#' @param ... parameters passed on to \code{as.data.frame.data.table}
#'
#' @return
#' @export
#'
#' @examples
as.data.frame.Stack_table <- function(dat, stack_method = 'row', ...){
  as.data.frame(as.data.table(dat, ...))
}

# Utility funs -----------------------------------------------------------------
stack_rows <- function(dat, insert_blank_row = FALSE){
  dat %assert_class% 'Stack_table'

  if(insert_blank_row){
    dat_blank <- rep('', nrow(dat[[1]]) * ncol(dat[[1]])) %>%
      `dim<-`(dim(dat[[1]])) %>%
      as.data.table()

    res <- data.table::rbindlist(list(dat[[1]], dat[[2]], dat_blank))
    res <- res[1:(nrow(res) - 1)]
  } else {
    res <- rbind(dat[[1]], dat[[2]])
  }

  roworder <- foreach(i = seq_len(nrow(dat[[1]])), .combine = c) %do% {
    if(insert_blank_row){
      c(i, i + nrow(dat[[1]]), i + 2L * nrow(dat[[1]]))
    } else {
      c(i, i + nrow(dat[[1]]))
    }
  }

  if(insert_blank_row){
    roworder <- roworder[-which(roworder == max(roworder))]
  }

  assert_that(max(roworder) %identical% nrow(res))
  return(res[roworder])
}


stack_rows_tex <- function(dat, insert_blank_row) {
  dat %assert_class% 'Stack_table'

  empty_row <- rep('', length(dat[[1]])) %>%
    t() %>%
    as.data.frame(stringsAsFactors = FALSE) %>%
    data.table::setnames(names(dat[[2]]))

  res <- foreach(i = 1:nrow(dat[[1]]), .combine = rbind) %do% {
    r <- paste(dat[[1]][i, ], dat[[2]][i, ], sep = ' \\newline ') %>%
      t() %>%
      as.data.frame()
    names(r) <-  names(dat[[1]])


    if (insert_blank_row && i != nrow(dat[[1]])) {
      r <- rbind(r, empty_row)
    }

    return(r)
  }
}


stack_cols <- function(dat, id_vars = NULL, suffixes = c('', '')){
  dat %assert_class% 'Stack_table'
  assert_that(length(suffixes) %identical% 2L)

  dd1 <- data.table::copy(dat[[1]])
  dd2 <- data.table::copy(dat[[2]])

  if (is.null(id_vars)){
    data.table::setnames(dd1, paste0(names(dd1), suffixes[[1]]))
    data.table::setnames(dd2, paste0(names(dd2), suffixes[[2]]))
    res <- cbind(dd1, dd2)
    start_order <- 1
  } else {
    res <- merge(dat[[1]], dat[[2]], by = id_vars, suffixes = suffixes)
    start_order <- length(id_vars) + 1
  }

  assert_that(start_order < ncol(dat[[1]]))

  colorder <- foreach(i = start_order:ncol(dat[[1]]), .combine = c) %do% {
    c(i, i + ncol(dat[[1]]) - (start_order - 1L))
  }
  colorder <- as.integer(c(seq_along(id_vars), colorder))
  assert_that(max(colorder) %identical% ncol(res))


  data.table::setcolorder(res, colorder)

  return(res)
}


stack_cols_tex <- function(dat) {
  foreach(i = 1:nrow(dat[[1]]), .combine = rbind) %do% {
    r <- paste(dat[[1]][i, ], dat[[2]][i, ], sep = ' ') %>%
      t() %>%
      as.data.frame()
    names(r) <-  names(dat[[1]])

    return(r)
  }
}
