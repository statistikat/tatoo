# Ctors -------------------------------------------------------------------

#' Mash Tables
#'
#' Mash tables are designed to make it easy to put together multidimensional
#' tables from two data.frames. An example where this might be useful is if you
#' have a data.frame of numeric values, and a second data.frame of associated
#' standard errors.
#'
#' Mash table provides a framework to mash those two data.frames together into
#' one data.frame with alternating rows or columns.
#'
#' @param ... `mash_table()` only: `data.frame`s with the same row and column
#'   count. Elements of `(...)` can be named, but the name must differ from
#'   the argument names of this function.
#' @param tables `mash_table_list()` only: a `list` of `data.frame`s as
#'   described for `(...)`
#' @param mash_method either `"row"` or `"col"`. should the tables be mashed by
#'   row or by column?
#' @param id_vars Only if mashing columns: one ore more colnames of the tables
#'   to be mashed. If supplied, columns of both input tables are combined with
#'   [merge()], otherwise [cbind()] is used.
#' @param insert_blank_row Only if mashing rows: logical. Whether to insert
#'   blank rows between mash-groups. *Warning: this converts all columns to
#'   character.* Use with care.
#' @param sep_height Only has an effect when exporting to `xlsx`. if
#'   `insert_blank_row == TRUE`, hight of the inserted row, else height of the
#'   top row of each mash-group.
#' @param meta A  [TT_meta] object. if supplied, output will also be a
#'   [Tagged_table].
#' @param rem_ext `character`. For `mash_table` to work, the column names of all
#'   elements of `dat` must be identical. Sometimes you will have the situation
#'   that column names are identical except for a suffix, such as `length` and
#'   `lenght.sd`. The `rem_ext` option can be used to remove such suffixes.
#'
#' @return a `Mashed_table`: a `list` of `data.table`s with additonal
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
#'
#' df_mean <- data.frame(
#'   Species = c("setosa", "versicolor", "virginica"),
#'   length = c(5.01, 5.94, 6.59),
#'   width = c(3.43, 2.77, 2.97)
#' )
#'
#' df_sd <- data.frame(
#'   Species = c("setosa", "versicolor", "virginica"),
#'   length = c(0.35, 0.52, 0.64),
#'   width = c(0.38, 0.31, 0.32)
#' )
#'
#'
#' # Mash by row
#'
#' mash_table(df_mean, df_sd)
#'
#' #       Species length width
#' # 1:     setosa   5.01  3.43
#' # 2:     setosa   0.35  0.38
#' # 3: versicolor   5.94  2.77
#' # 4: versicolor   0.52  0.31
#' # 5:  virginica   6.59  2.97
#' # 6:  virginica   0.64  0.32
#'
#'
#' # Mash by column
#'
#' mash_table(
#'   df_mean, df_sd,
#'   mash_method = 'col',
#'   id_vars = 'Species'
#' )
#'
#' #       Species    Species length length width width
#' # 1:     setosa     setosa   5.01   0.35  3.43  0.38
#' # 2: versicolor versicolor   5.94   0.52  2.77  0.31
#' # 3:  virginica  virginica   6.59   0.64  2.97  0.32
#'
#'
#' # Use the id_vars argument to prevent undesired dpulicated columns,
#' # and name the input data.frames to get multi-col headings.
#'
#' mash_table(
#'   mean = df_mean, sd = df_sd,
#'   mash_method = 'col',
#'   id_vars = 'Species'
#' )
#'
#' #    ..........     ..length...     ...width...
#' # 1    Species     mean     sd     mean     sd
#' # 2     setosa     5.01   0.35     3.43   0.38
#' # 3 versicolor     5.94   0.52     2.77   0.31
#' # 4  virginica     6.59   0.64     2.97   0.32
#'
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

#' @export
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




#' @rdname Mashed_table
#' @export
is_Mashed_table <- function(dat, ...){
  inherits(dat, 'Mashed_table')
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

  print_multi_headings <-
    attr(dat, 'mash_method') %identical% 'col' &&
    length(names(dat)) %identical% length(dat)


  if(print_multi_headings){
    pdat <- as_Composite_table(dat, meta = NULL)
    lines <- capture.output(print(pdat, ...))
  } else {
    lines <- capture.output(print(
      as.data.table(dat, insert_blank_row = FALSE),
      ...
    ))
  }


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
#'
#' @return a \code{data.table} or \code{data.frame}
#' @export
as.data.table.Mashed_table <- function(
  dat,
  mash_method = attr(dat, 'mash_method'),
  insert_blank_row = attr(dat, 'insert_blank_row'),
  id_vars = attr(dat, 'id_vars'),
  suffixes = names(dat)
){
  assert_that(purrr::is_scalar_character(mash_method))
  assert_that(is.flag(insert_blank_row))
  assert_that(is.null(id_vars) || is.character(id_vars))
  assert_that(is.null(suffixes) || is.character(suffixes) )
  assert_that(is.null(suffixes) || length(suffixes) %identical% length(dat))

  if(mash_method %in% c('c', 'col', 'column', 'columns')){
    res <- mash_cols(dat, id_vars = id_vars)
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
  suffixes = names(dat)
){
  as.data.frame(as.data.table.Mashed_table(dat))
}




#  Shortcut functions -----------------------------------------------------

#' Mash R objects by Rows or Columns
#'
#' `rmash` and `cmash` are convience function to mash `data.frames` together
#' with a single command. They behave similar to [cbind()] and
#' [rbind()], just that the result will have have alternating rows/colums.
#'
#' @param ... either several \code{data.frames} or a single [Mashed_table].
#'   All `data.frames` must have the same number of columns.
#' @inheritParams mash_table
#' @inheritParams as.data.table.Mashed_table
#'
#' @return A [data.table] if
#'   any element of `(...)` is a data.table
#'   or [Tatoo_table],
#'   or if `meta` is supplied;
#'   else a `data.frame`.
#'
#' @md
#' @rdname cmash
#' @seealso [Mashed_table]
#' @export
#'
#' @examples
#'
#' dat1 <- data.frame(
#'   x = 1:3,
#'   y = 4:6
#' )
#'
#' dat2 <- data.frame(
#'   x = letters[1:3],
#'   y = letters[4:6]
#' )
#'
#' rmash(dat1, dat2)
#'
#' #    x y
#' # 1: 1 4
#' # 2: a d
#' # 3: 2 5
#' # 4: b e
#' # 5: 3 6
#' # 6: c f
#'
#' cmash(dat1, dat2)
#'
#' #    x x y y
#' # 1: 1 a 4 d
#' # 2: 2 b 5 e
#' # 3: 3 c 6 f
#'
#'
rmash <- function(
  ...,
  rem_ext = NULL,
  insert_blank_row = FALSE,
  meta = NULL
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

  input_contains_dt <- lapply(dots, tt_or_dt) %>%
    unlist() %>%
    any()

  if (!is.null(meta)){
    meta(res) <- meta
  } else if (!input_contains_dt) {
    res <- as.data.frame(res)
  }

  return(res)
}




#' @export
#' @rdname cmash
cmash <- function(
  ...,
  rem_ext = NULL,
  id_vars = NULL,
  suffixes = names(list(...)),
  meta = NULL
){
  dots <- list(...)


  input_is_Mashed_table <-
    purrr::is_scalar_list(dots)
    is_Mashed_table(dots)

  if(input_is_Mashed_table){
    res <- dots[[1]] %>%
      as.data.table(mash_method = 'col', suffixes = suffixes)

  } else {
    res <- dots %>%
      mash_table_list(
        mash_method = 'col',
        id_vars = id_vars,
        rem_ext = rem_ext) %>%
      as.data.table(suffixes = suffixes)
  }


  input_contains_dt <- lapply(dots, tt_or_dt) %>%
    unlist() %>%
    any()

  if (!is.null(meta)){
    meta(res) <- meta
  } else if (!input_contains_dt) {
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
  assert_that(looks_like_integer(value))

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


  dd <- lapply(dat, df_typecast_all, from = 'factor', to = 'character')

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

  # Remove trailing blank line
  if(insert_blank_row){
    res <- res[-nrow(res)]
  }

  return(res)
}




mash_cols <- function(
  dat,
  id_vars = NULL,
  suffixes = names(dat),
  sep = '.'
){
  # Preconditions
    dat %assert_class% 'Mashed_table'
    assert_that(
      is.null(id_vars) ||
      is.character(id_vars)
    )

    if (is.null(suffixes)) {
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
    suffix    <- suffixes[[i]]

    if (!(is.null(suffix) || (suffix %identical% ''))){
      suffix <- paste0(sep, suffix)
    } else {
      suffix <- ''
    }

    new_names[!new_names %in% id_vars] <- paste0(
      new_names[!new_names %in% id_vars],
      suffix
    )
    data.table::setnames(dl[[i]], new_names)
  }


  # Flatten
  if (is.null(id_vars)){
    names(dl) <- NULL
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



tt_or_dt <- function(x) {
  data.table::is.data.table(x) || is_Tatoo_table(x)
}
