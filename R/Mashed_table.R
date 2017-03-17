Mashed_table <- function(
  dat,
  mash_method
){
  assert_that(is.list(dat))
  assert_that(
    identical(mash_method, 'row') ||
    identical(mash_method, 'col')
  )

  res <- data.table::copy(dat) %>%
    tatoo_table()
  data.table::setattr(res, 'class', union('Mashed_table', class(res)))
  data.table::setattr(res, 'mash_method', mash_method)
  return(res)
}




#' Mash Table
#'
#' Stack tables are designed to make it easy to put together multidimensional
#' tables from two data.frames. An example where this might be useful is
#' if you have a data.frame of numeric values, and a second data.frame of
#' associated standard errors.
#'
#' Stack table provides a framework to stack those two data.frames together
#' into one data.frame with alternating rows or columns. You can then output the
#' stacked table as latex \code{\link{print_tex.Mashed_table}}, as xlsx
#' \code{\link{save_as.Mashed_table}} or simply as data.table or data.frame
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
#' @rdname mash_table
#'
#' @examples
mash_table <- function(
  ...,
  rem_ext = NULL,
  mash_method = 'row',
  meta = NULL
  ){
  mash_table_list(
    list(...),
    rem_ext = rem_ext,
    mash_method = mash_method,
    meta = meta
  )
}




#' @export
#' @rdname mash_table
mash_table_list <- function(
  tables,
  mash_method = 'row',
  rem_ext = NULL,
  meta = NULL
){
  assert_that(is.list(tables))
  assert_that(is.null(meta) || is_class(meta, 'TT_meta'))

  # Check Inputs
  assert_that(length(tables) > 1)
  assert_that(all(
    unlist(lapply(tables, is.data.frame))
  ))

  for (table in tables) {
    assert_that(nrow(table) %identical% nrow(tables[[1]]))
    assert_that(ncol(table) %identical% ncol(tables[[1]]))
  }

  assert_that(
    is.null(rem_ext) ||
      purrr::is_scalar_character(rem_ext)
  )


  # Process inputs
  res <- lapply(tables, function(x) {
    data.table::as.data.table(data.table::copy(x))
  })

  if(!is.null(rem_ext)){
    res <- lapply(res, function(x) {
      data.table::setnames(x, gsub(rem_ext, '', names(x)))
    })
  }

  res <- lapply(res, function(x) {
    data.table::setcolorder(x, names(res[[1]]))
  })


  # Post conditions
  for (el in res) {
    assert_that(names(el) %identical% names(tables[[1]]))
    assert_that(data.table::is.data.table(el))
    assert_that(nrow(el) %identical% nrow(tables[[1]]))
    assert_that(ncol(el) %identical% ncol(tables[[1]]))
  }


  res <- Mashed_table(
    res,
    mash_method = mash_method
  )

  if(!is.null(meta)){
    res <- tag_table(res, meta = meta)
  }

  return(res)
}




#' @export
as_mash_table <- function(dat, ...){
  UseMethod('as_mash_table')
}



#' @param ... either several \code{data.frames} or a single \code{Mashed_table}.
#' @param rem_ext
#' @param insert_blank_row whether or not to insert a blank row between mash paris
#'
#' @export
#' @rdname mash_table
rmash <- function(
  ...,
  rem_ext = NULL,
  insert_blank_row = FALSE
){
  dots <- list(...)

  if(length(dots) %identical% 1L && is_class(dots[[1]], 'Mashed_table')){
    res <- dots[[1]] %>%
      as.data.table(
        mash_method = 'row',
        insert_blank_row = insert_blank_row
      )
  } else {
    res <- mash_table_list(dots, rem_ext = rem_ext) %>%
      as.data.table(
        mash_method = 'row',
        insert_blank_row = insert_blank_row
    )
  }

  assert_that(identical(
    class(res),
    c('data.table', 'data.frame')
  ))

  return(res)
}




#' @export
#' @rdname mash_table
cmash <- function(
  ...,
  rem_ext = NULL,
  by = NULL,
  suffixes = NULL,
  meta = NULL
){
  dots <- list(...)

  if(length(dots) %identical% 1L && is_class(dots[[1]], 'Mashed_table')){
    res <- dots[[1]] %>%
      as.data.table(
        mash_method = 'col',
        suffixes = suffixes
      )
    meta(res) <- attr(dots[[1]], 'meta')
  } else {
    res <-  mash_table_list(dots, rem_ext = rem_ext) %>%
      as.data.table(
        mash_method = 'col',
        by = by,
        suffixes = suffixes
    )
  }

  return(res)
}




#' Title
#'
#' @param dat
#' @param mash_method
#' @param ... passed on to as.data.frame.data.table
#'
#' @return
#' @export
#'
#' @examples
as.data.table.Mashed_table <- function(
  dat,
  mash_method = 'row',
  insert_blank_row = (mash_method == 'row'),
  by = NULL,
  suffixes = NULL
){
  assert_that(purrr::is_scalar_character(mash_method))
  assert_that(is.flag(insert_blank_row))

  assert_that(is.scalar(stack))
  if(mash_method %in% c('c', 'col', 'column', 'columns')){
    assert_that(insert_blank_row %identical% FALSE)
    res <- mash_cols(dat, by = by, suffixes = suffixes)
  } else if(mash_method %in% c('r', 'row', 'rows')) {
    res <- mash_rows(dat, insert_blank_row = insert_blank_row)
  } else{
    stop('mash_method must be either "row" or "col".')
  }

  return(as.data.table(res))
}




#' Title
#'
#' Stacking uses \code{data.table}s internally,
#'
#' @param dat
#' @param mash_method
#' @param ... parameters passed on to \code{as.data.frame.data.table}
#'
#' @return
#' @export
#'
#' @examples
as.data.frame.Mashed_table <- function(dat, mash_method = 'row', ...){
  as.data.frame(as.data.table(dat))
}




# Utility funs -----------------------------------------------------------------
mash_rows <- function(dat, insert_blank_row = FALSE){
  dat %assert_class% 'Mashed_table'
  assert_that(is.flag(insert_blank_row))


  dd <- lapply(dat, hammr::df_typecast_all, from = 'factor', to = 'character')

  # flatten
    if(insert_blank_row){
      blank_rowks <- rep('', nrow(dd[[1]]) * ncol(dd[[1]])) %>%
        `dim<-`(dim(dd[[1]])) %>%
        as.data.table()

      dl <- c(dd, list(blank_rowks))

      res <- data.table::rbindlist(dl)
    } else {
      res <- data.table::rbindlist(dd)
    }


  for(i in which(unlist(lapply(dat[[1]], is.factor)))){
    res[[i]] <- as.factor(res[[i]])
  }


  # Determine output row order ("mash")
    roworder <- seq_len((length(dat) + insert_blank_row) * nrow(dd[[1]]))
    roworder <- matrix(
      roworder,
      nrow = nrow(dd[[1]]),
      ncol = (length(dd) + insert_blank_row)
    )
    roworder <- as.vector(t(roworder))


  # Post condtions
    assert_that(identical(
      max(roworder),
      nrow(res)
    ))


  # Output
    res <- res[roworder]

    ## Remove trailing blank line
    if(insert_blank_row){
      res <- res[-nrow(res)]
    }

    return(res)
}




mash_cols <- function(
  dat,
  by = NULL,
  suffixes = names(dat)
){
  # Preconditions
    dat %assert_class% 'Mashed_table'
    assert_that(
      is.null(by) ||
      is.character(by)
    )

    if (is.null(names(dat)) && is.null(suffixes)) {
      suffixes <- rep('', length(dat))
    } else {
      assert_that(length(suffixes) %identical% length(dat))
    }


  # Prepare inputs
    dl <- lapply(dat, function(x){
      data.table::copy(x)
    })

    for(i in seq_along(dl)){
      new_names <- names(dl[[i]])
      new_names[!new_names %in% by] <- paste0(
          new_names[!new_names %in% by],
          suffixes[[i]]
      )
      data.table::setnames(dl[[i]], new_names)
    }


  # Flatten
    if (is.null(by)){
      res <- do.call(cbind, dl)
    } else {
      merger <- function(x, y)  {
        suppressWarnings(
          merge.data.frame(
            x,
            y,
            by = by,
            all = TRUE,
            sort = FALSE,
            suffixes = c('', ''))
        )
      }
      res <- Reduce(merger, dl) %>%
        as.data.table()
    }

  # Determine output col order
    colorder <- seq_len(
      length(dat) * (ncol(dat[[1]]) - length(by))
    )

    colorder <- matrix(
      colorder,
      nrow = (ncol(dat[[1]]) - length(by)),
      ncol = length(dat)
    )

    colorder <- as.vector(t(colorder))

    if(length(by) > 0){
      i_by <- seq_along(by)
      colorder <- colorder + max(i_by)
      colorder <- c(i_by, colorder)
    }


    assert_that(identical(
      max(colorder),
      ncol(res)
    ))


  # Output
  data.table::setcolorder(res, colorder)
  data.table::setattr(res, 'by', by)
  return(res)
}



#' Printing Mashed Tables
#'
#' @param dat a \code{Mashed_table}
#' @param ... passed on to \code{\link{print}}
#'
#' @return \code{dat} (invisibly)
#'
#' @export
print.Mashed_table <- function(dat, ...){
  print(as.data.table(dat), ...)
  invisible(dat)
}



#' Set the multinames attribute of a Composite_table
#'
#' @param dat a Composite_table or data.frame
#' @param value a named character vector (see example)
#'
#' @export
`mash_method<-` <- function(dat, value){
  dat %assert_class% 'Mashed_table'

  res <- data.table::copy(dat)
  data.table::setattr(res, 'mash_method', value)

  return(res)
}
