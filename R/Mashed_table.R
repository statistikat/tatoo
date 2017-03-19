# Ctors -------------------------------------------------------------------

#' Mash Tables
#'
#' Mash tables are designed to make it easy to put together multidimensional
#' tables from two data.frames. An example where this might be useful is
#' if you have a data.frame of numeric values, and a second data.frame of
#' associated standard errors.
#'
#' Mash table provides a framework to mash those two data.frames together
#' into one data.frame with alternating rows or columns.
#'
#' If your goal is to present formated tables as latex or xlsx, you  should aslo
#' look into `\link{df_format}`, `\link{df_round}` and
#' `\link{df_signif}`.
#'
#' @param ... `mash_table()` only: `data.frame`s with the same row and column
#'   count.
#' @param `mash_table_list()` only: a `list` of `data.frame`s as described for
#'   `(...)`
#' @param mash_method either `"row"` or `"col"`. should the tables be mashed by
#'   row or by column?
#' @param id_vars Only if `mash_method == "col"`: If supplied, columns of both
#'   input tables are combined with `\link{merge}`, otherwise `\link{cbind}` is
#'   used.
#' @param insert_blank_row logical. Whether to insert blank rows between
#'   \code{data.frame} columns. Warning: this converts all columns to character.
#' @param sep_height Only has an effect when exporting to `xlsx`. if
#'   `insert_blank_row == TRUE`, hight of the inserted row, else height of the
#'   top row of each mash-group.
#' @param meta A `\link{TT_meta} object. if supplied, output will also be a
#'   `\link{Tagged_table}`
#' @param rem_ext `character`. For `mash_table` to work, the column names of
#'   all elements of `dat` must be identical. Sometimes you will have the
#'   situation that column names are identical except for a suffix, such as
#'   `length` and `lenght.sd`. The `rem_ext` option can be used to remove
#'   such suffixes.
#'
#' @return a \code{Mashed_table}: a `list` of `data.table`s with additonal
#'   `mash_method`, `insert_blank_row` and `sep_height` attributes, that
#'   influence how the table looks when it is printed or exported.
#'
#' @md
#' @rdname Mashed_table
#' @aliases Mashed_table mashed_table mash_table
#' @family Tatto tables
#' @export
#'
#' @examples
mash_table <- function(
  ...,
  mash_method = 'row',
  id_vars = NULL,
  insert_blank_row = FALSE,
  sep_height = 24,
  meta = NULL,
  rem_ext = NULL
){
  mash_table_list(
    list(...),
    mash_method = mash_method,
    id_vars = id_vars,
    insert_blank_row = insert_blank_row,
    sep_height = sep_height,
    meta = meta,
    rem_ext = rem_ext
  )
}







#' @export
#' @rdname Mashed_table
mash_table_list <- function(
  tables,
  mash_method = 'row',
  id_vars = NULL,
  insert_blank_row = FALSE,
  sep_height = 24,
  meta = NULL,
  rem_ext = NULL
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
    mash_method = mash_method,
    id_vars = id_vars,
    insert_blank_row = insert_blank_row,
    sep_height = sep_height
  )

  if(!is.null(meta)){
    res <- tag_table(res, meta = meta)
  }

  return(res)
}




Mashed_table <- function(
  dat,
  mash_method = 'row',
  id_vars = NULL,
  insert_blank_row = FALSE,
  sep_height = 24
){
  assert_that(is.list(dat))
  assert_that(
    identical(mash_method, 'row') ||
    identical(mash_method, 'col')
  )
  assert_that(is.number(sep_height))
  assert_that(looks_like_integer(sep_height))

  sep_height <- as.integer(sep_height)

  res <- data.table::copy(dat) %>%
    tatoo_table()

  data.table::setattr(res, 'class', union('Mashed_table', class(res)))
  data.table::setattr(res, 'mash_method', mash_method)
  data.table::setattr(res, 'id_vars', id_vars)
  data.table::setattr(res, 'insert_blank_row', insert_blank_row)
  data.table::setattr(res, 'sep_height', sep_height)

  assert_valid(res)
  return(res)
}




# Methods -----------------------------------------------------------------

is_valid.Mashed_table <- function(dat){
  res <- list(
    is_list          = is.list(dat),
    mash_method      = identical(attr(dat, 'mash_method'), 'row') ||
                       identical(attr(dat, 'mash_method'), 'col'),
    id_vars               = is.null(attr(dat, 'id_vars')) ||
                       is.character(attr(dat, 'id_vars')),
    insert_blank_row = is.flag(attr(dat, 'insert_blank_row')),
    sep_height       = purrr::is_scalar_integer(attr(dat, 'sep_height'))
  )

  all_with_warning(res)
}


is_Mashed_table <- function(dat, ...){
  inherits(dat, 'Mashed_table') && is_valid(dat)
}

#' @export
as_mashed_table <- function(dat, ...){
  UseMethod('as_mashed_table')
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
  lines <- capture.output(print(
    as.data.table(dat, insert_blank_row = FALSE),
    ...
  ))

  for(i in seq_along(lines)){
    cat(lines[[i]], '\n')

    if(attr(dat, 'insert_blank_row') &&
       attr(dat, 'mash_method') %identical% 'row'
    ){
      insert_blank <-
         i > length(dat) &&
        (i-1) %% length(dat) == 0 &&
        !identical(i, length(lines))

      assert_that(is.flag(insert_blank))

      if(insert_blank){
        cat('\n')
      }
    }
  }

  invisible(dat)
}




#' Convert a Mashed Table to a data.table or data.frame
#'
#' @param dat a \code{Mashed_table}
#' @inheritParams mash_table
#' @param suffixes Only if \code{mash_method == "col"}: a character vector of
#'   the same length as \code{dat}. Suffixes to append to each column
#'
#' @return a \code{data.table} or \code{data.frame}
#' @export
#'
#' @examples
as.data.table.Mashed_table <- function(
  dat,
  mash_method = attr(dat, 'mash_method'),
  insert_blank_row = attr(dat, 'insert_blank_row'),
  id_vars = attr(dat, 'id_vars'),
  suffixes = NULL
){
  assert_that(purrr::is_scalar_character(mash_method))
  assert_that(is.flag(insert_blank_row))
  assert_that(is.null(id_vars) || is.character(id_vars))
  assert_that(is.null(suffixes) || is.character(suffixes) )
  assert_that(is.null(suffixes) || length(suffixes) %identical% length(dat))

  if(mash_method %in% c('c', 'col', 'column', 'columns')){
    res <- mash_cols(dat, id_vars = id_vars, suffixes = suffixes)
  } else if(mash_method %in% c('r', 'row', 'rows')) {
    res <- mash_rows(dat, insert_blank_row = insert_blank_row)
  } else{
    stop('mash_method must be either "row" or "col".')
  }

  return(as.data.table(res))
}




#' @rdname as.data.table.Mashed_table
#' @export
as.data.frame.Mashed_table <- function(
  dat,
  mash_method = attr(dat, 'mash_method'),
  insert_blank_row = attr(dat, 'insert_blank_row'),
  id_vars = attr(dat, 'id_vars'),
  suffixes = NULL
){
  as.data.frame(as.data.table.Mashed_table(dat))
}




#  Shortcut functions -----------------------------------------------------

#' Mash R objects by Rows or Columns
#'
#'
#'
#' @param ... either several \code{data.frames} or a single \code{Mashed_table}.
#'
#' @param rem_ext
#' @param insert_blank_row `rbind` onlywhether or not to insert a blank row between mash paris
#' @param data.table
#'
#' @export
#' @rdname rmash
rmash <- function(
  ...,
  rem_ext = NULL,
  insert_blank_row = FALSE,
  data.table = TRUE
){
  dots <- list(...)

  input_is_Mashed_table <-
    purrr::is_scalar_list(dots)
    is_Mashed_table(dots)

  if(input_is_Mashed_table){
    res <- dots[[1]] %>%
      as.data.table(
        mash_method = 'row',
        insert_blank_row = insert_blank_row
      )

  } else {
    res <- dots %>%
      mash_table_list(
        rem_ext = rem_ext,
        mash_method = 'row',
        insert_blank_row = insert_blank_row) %>%
      as.data.table()
  }

  assert_that(identical(
    class(res),
    c('data.table', 'data.frame')
  ))

  if(!data.table){
    res <- as.data.frame(res)
  }

  return(res)
}




#' @export
#' @rdname rmash
cmash <- function(
  ...,
  rem_ext = NULL,
  id_vars = NULL,
  suffixes = NULL,
  data.table = TRUE
){
  dots <- list(...)

  input_is_Mashed_table <-
    purrr::is_scalar_list(dots)
    is_Mashed_table(dots)

  if(input_is_Mashed_table){
    res <- dots[[1]] %>%
      as.data.table(
        mash_method = 'col',
        suffixes = suffixes
      )
  } else {
    res <-  dots %>%
      mash_table_list(
        mash_method = 'col',
        id_vars = id_vars,
        rem_ext = rem_ext) %>%
      as.data.table(suffixes = suffixes)
  }

  if(!data.table){
    res <- as.data.frame(res)
  }

  return(res)
}




# Setters -----------------------------------------------------------------

#' @rdname Mashed_table
#' @export
`mash_method<-` <- function(dat, value){
  dat %assert_class% 'Mashed_table'
  assert_that(identical(value, 'row') || identical(value, 'col'))

  res <- data.table::copy(dat)

  data.table::setattr(res, 'mash_method', value)
  return(res)
}

#' @rdname Mashed_table
#' @export
`insert_blank_row<-` <- function(dat, value){
  dat %assert_class% 'Mashed_table'
  assert_that(is.flag(value))

  res <- data.table::copy(dat)

  data.table::setattr(res, 'insert_blank_row', value)
  return(res)
}

#' @rdname Mashed_table
#' @export
`sep_height<-` <- function(dat, value){
  dat %assert_class% 'Mashed_table'
  assert_that(hammr::looks_like_integer(value))

  value <- as.integer(value)
  res <- data.table::copy(dat)

  data.table::setattr(res, 'sep_height', value)
  return(res)
}

#' @rdname Mashed_table
#' @export
`id_vars<-` <- function(dat, value){
  dat %assert_class% 'Mashed_table'
  assert_that(is.character(value))

  res <- data.table::copy(dat)

  data.table::setattr(res, 'id_vars', value)
  return(res)
}






# Utils -------------------------------------------------------------------

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
  id_vars = NULL,
  suffixes = names(dat)
){
  # Preconditions
  dat %assert_class% 'Mashed_table'
  assert_that(
    is.null(id_vars) ||
      is.character(id_vars)
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
    new_names[!new_names %in% id_vars] <- paste0(
      new_names[!new_names %in% id_vars],
      suffixes[[i]]
    )
    data.table::setnames(dl[[i]], new_names)
  }


  # Flatten
  if (is.null(id_vars)){
    res <- do.call(cbind, dl)
  } else {
    merger <- function(x, y)  {
      suppressWarnings(
        merge.data.frame(
          x,
          y,
          by = id_vars,
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
    length(dat) * (ncol(dat[[1]]) - length(id_vars))
  )

  colorder <- matrix(
    colorder,
    nrow = (ncol(dat[[1]]) - length(id_vars)),
    ncol = length(dat)
  )

  colorder <- as.vector(t(colorder))

  if(length(id_vars) > 0){
    i_id_vars <- seq_along(id_vars)
    colorder <- colorder + max(i_id_vars)
    colorder <- c(i_id_vars, colorder)
  }


  assert_that(identical(
    max(colorder),
    ncol(res)
  ))


  # Output
  data.table::setcolorder(res, colorder)
  data.table::setattr(res, 'id_vars', id_vars)
  return(res)
}



