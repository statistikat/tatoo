# Ctors -------------------------------------------------------------------

#' Compose Tables
#'
#' `comp_table()` is a drop in replacement for [base::cbind()] that supports
#' multi-column headings.#'
#'
#' @param ... `comp_table()` only: individual `data.frames`. A name can be
#'   provided for each `data.frame` that will be used by [print()] and
#'   [as_workbook()] to create multi-table headings.
#' @param id_vars If `id_vars` is specified, the tables will be combined
#'   using [merge()] on the columns specified in `id_vars`, otherwise
#'   the tables will be combined with [cbind()].
#' @param meta a [TT_meta] object. If specified, the resulting
#'   `Composite_table` will be wrapped in a [Tagged_table].
#'
#' @return A `Composite_table`.
#'
#' @rdname Composite_table
#' @aliases comp_table composite_table Composite_table
#' @family Tatoo tables
#' @seealso Attribute setter: [multinames<-]
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
#' comp_table(mean = df_mean, sd = df_sd)
#'
#' # ...........mean............     ............sd.............
#' # 1    Species   length   width        Species   length   width
#' # 2     setosa     5.01    3.43         setosa     0.35    0.38
#' # 3 versicolor     5.94    2.77     versicolor     0.52    0.31
#' # 4  virginica     6.59    2.97      virginica     0.64    0.32
#'
#'
#' comp_table(mean = df_mean, sd = df_sd, id_vars = 'Species')
#'
#' # ..........     .....mean.....     ......sd......
#' # 1    Species     length   width     length   width
#' # 2     setosa       5.01    3.43       0.35    0.38
#' # 3 versicolor       5.94    2.77       0.52    0.31
#' # 4  virginica       6.59    2.97       0.64    0.32
#'
comp_table <- function(
  ...,
  id_vars = NULL,
  meta = NULL
){
  dots <- list(...)

  is_named_list <- identical(length(names(dots)), length(dots))
  if(!is_named_list){
    names(dots) <- get_dot_names(...)
  }

  comp_table_list(
    tables = dots,
    id_vars = id_vars,
    meta = meta
  )
}




#' @param tables \code{comp_table_list} only: A named list of data.frames with
#'   the same number of rows
#'
#' @rdname Composite_table
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
    names(tables) <- paste('tab', seq_along(tables))
  }

  # Combine the tables
  if(is.null(id_vars)){
    res <- do.call(cbind, tables) %>%
      stats::setNames(unlist(lapply(tables, names)))

  } else {
    id_vars_in_colnames <- tables %>%
      lapply(names) %>%
      vapply(function (x) id_vars %in% x, logical(1))

    assert_that(all(id_vars_in_colnames))

    merger <- function(x, y) {
      suppressWarnings(
      merge.data.frame(
        x,
        y,
        by = id_vars,
        all = TRUE,
        suffixes = c('', ''),
        sort = FALSE))
    }
    res <- Reduce(merger, tables)
  }

  # Generate table-title cell positions (for xlsx / latex export).
  # if a "id_vars" was specified, this has to be considered when creating the
  # indices
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
  assert_that(all(is_integerish(multinames)))

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




#' Coerce to Composite Table
#'
#' Converts other \R objects to `Composite_tables` by automatically creating
#' multi-column names from the properties of the objects.
#'
#' @template any_r
#' @param ... Ignored
#'
#' @return `as_Composte_table()` returns a `Composite_table`
#'
#' @export
as_Composite_table <- function(x, ...){
  UseMethod('as_Composite_table')
}




#' `as_Composite_table.Mashed_table()` extracts the multi-column names from the
#'   column names of the individual data.frames that make up a [Mashed_table],
#'   and the column names from the names of the Mashed Table.
#'
#' @inheritParams comp_table
#'
#' @rdname as_Composite_table
#' @export
#' @examples
#'
#' mash_table(
#'   head = head(cars),
#'   tail = tail(cars),
#'   mash_method = 'col'
#' )
#'
as_Composite_table.Mashed_table <- function(
  x,
  id_vars = attr(x, 'id_vars'),
  meta = attr(x, 'meta'),
  ...
){
  assert_that(length(names(x)) %identical% length(x))
  assert_that(is.null(id_vars) || all(id_vars %in% names(x[[1]])))

  col_names <- names(x[[1]])[! names(x[[1]]) %in% id_vars]
  n_idv  <- length(id_vars)
  n_tbls <- length(x)
  n_cols <- ncol(x[[1]]) - n_idv

  if(n_idv > 0){
    multinames        <- cumsum(c(n_idv, rep(n_tbls, n_cols)))
    names(multinames) <- c('', col_names)
  } else {
    multinames        <- cumsum(c(rep(n_tbls, n_cols)))
    names(multinames) <- names(x[[1]])
  }

  res <- data.table::as.data.table(x, mash_method = 'col', id_vars = id_vars)
  names(res) <- c(id_vars, rep(names(x), n_cols))
  multinames(res) <- multinames
  meta(res) <- meta

  return(res)
}




#' `as_Composite_table.data.frame()` extracts the multi-column names from the
#'   column names of a `data.frame` based on a separator.
#'
#' @param sep a scalar character. Separator in the column names of `x` that
#'   separates the column name from the multi-column name.
#' @param reverse logical. if `FALSE` the part after the last occurrence of `sep`
#'   will be used as multiname, if `TRUE` the part before will be used.
#'
#' @export
#' @rdname as_Composite_table
#'
#' @examples
#'
#' as_Composite_table(data.frame(
#'   apple.fruit = 1,
#'   kiwi.fruit = 2,
#'   dog.animal = 1,
#'   black.cat.animal = 2,
#'   parrot.animal = 3
#' ))
#'
as_Composite_table.data.frame <- function(
  x,
  sep = ".",
  reverse = FALSE,
  ...
){
  as_Composite_table(
    as.data.table(x),
    sep = sep,
    reverse = reverse
  )
}




#' @export
as_Composite_table.data.table <- function(
  x,
  sep = ".",
  reverse = FALSE,
  ...
){
  assert_that(is_scalar_character(sep))
  assert_that(is.flag(reverse))

  # Process inputs
    res <- data.table::copy(x)


  # Logic
    splt_pos <- stringi::stri_locate_last_fixed(names(x), sep)[, 1]
    cnames <- stringi::stri_sub(names(x), to = splt_pos - 1)
    mnames <- stringi::stri_sub(names(x), from = splt_pos + 1)

    if(reverse){
      tmp <- cnames
      cnames <- mnames
      mnames <- tmp
    }

    sel <- is.na(cnames)
    cnames[sel] <- names(x)[sel]
    mnames[is.na(mnames)] <- ''

  res %>%
    data.table::setnames(cnames) %>%
    Composite_table(as_multinames(mnames))
}




#' @templateVar fun is_Composite_table
#' @templateVar class Composite_table
#' @template is_class
#'
#' @rdname as_Composite_table
#' @export
is_Composite_table <- function(x, ...){
  inherits(x, 'Composite_table')
}




# Methods -----------------------------------------------------------------

#' Printing Composite Tables
#'
#' @param x a `Composite_table`
#' @param right Logical. Should strings be right aligned? The default is
#'   left-alignment (the opposite of the standard [print.data.frame()]).
#' @param ... passed on to \code{\link{print}}
#'
#' @return \code{x} (invisibly)
#'
#' @export
print.Composite_table <- function(
  x,
  right = FALSE,
  ...
){
  lines <- as_lines(x, ...)
  lines <- strip_newlines(lines)
  cat(lines, sep = "\n")

  invisible(x)
}




#' Convert a Composite Table to a data.table or data.frame
#'
#' As a `Composite_table` already is a `data.table` this function
#' does very little except stripping all additional attributes and classes,
#' as well as offering you the option to prepend the `multinames` before
#' the column names
#'
#' @param x a \code{Composite_table}
#' @param keep.rownames ignored
#' @param multinames logical. Whether to prepend multinames before the column
#'   names
#' @param sep separator between multinames and individual column names
#' @param ... ignored
#'
#' @return a \code{data.table} or \code{data.frame}
#'
#' @method as.data.table Composite_table
#' @export
as.data.table.Composite_table <- function(
  x,
  keep.rownames = NULL,
  ...,
  multinames = TRUE,
  sep = '.'
){
  # Preconditions
    assert_that(is.flag(multinames))
    assert_that(is_scalar_character(sep))
    assert_that(is_scalar_character(sep))
    assert_rownames_is_null(keep.rownames)


  # Process arguments
    x <- data.table::copy(x)


  # Logic
    if(!multinames || is.null(multinames(x))){
      data.table::setattr(x, 'class', c('data.table', 'data.frame'))
      return(as.data.table(x))
    } else {
      res <- x
      multinames <- attr(res, 'multinames')
      name_idx <- 1


      # paste together colname
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


  # Cleanup
    data.table::setattr(res, 'class', c('data.table', 'data.frame'))
    return(as.data.table(res))
  }
}




#' @rdname as.data.table.Composite_table
#'
#' @inheritParams base::as.data.frame
#' @method as.data.frame Composite_table
#' @export
as.data.frame.Composite_table <- function(
  x,
  row.names = NULL,
  optional = FALSE,
  ...,
  multinames = TRUE,
  sep = '.'
){
  assert(is.null(row.names), "row.names argument must be null. Please name all arguments to as.data.frame.Composite_table explictely")
  as.data.frame(
    as.data.table.Composite_table(
      x = x,
      multinames = multinames,
      sep = sep
    ),
    row.names = row.names,
    optional = optional,
    ...
  )
}




# Setters -----------------------------------------------------------------

#' Set the multinames attribute of a Composite_table
#'
#' @param x a `Composite_table` `or data.frame`
#' @param value a named vector of ascending integers. The name is the
#'   multi-column heading, the integer value is the last column that this
#'   heading applies to
#'
#' @seealso [Composite_table], [as_multinames()]
#' @rdname multinames
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
#' multinames(df_mean) = c("species" = 1, measures = 3)
#'
#' # .species..     ...measures...
#' # 1    Species     length   width
#' # 2     setosa       5.01    3.43
#' # 3 versicolor       5.94    2.77
#' # 4  virginica       6.59    2.97
#'
#'
`multinames<-` <- function(x, value){
  assert_that(is.data.frame(x))
  assert_that(max(value) == ncol(x))

  if(is_class(x, 'Composite_table')){
    dd  <- data.table::copy(x)
    res <- data.table::setattr(dd, 'multinames', value)
  } else {
    res <- Composite_table(x, multinames = value)
  }

  return(res)
}




#' @rdname multinames
#' @export
multinames <- function(x){
  assert_that(inherits(x, 'Composite_table'))
  attr(x, 'multinames')
}




# Utils -------------------------------------------------------------------

composite_name <- function(x, y, sep){
  if(x == ''){
    return(y)
  } else {
    paste(y, x, sep = sep)
  }
}




#' Create Composite Table multinames from a character vector
#'
#' @param x a character vector of equal length as the data.frame for which it
#'   the multinames should be created.
#'
#' @return a named integer vector that can be used as multinames attribute
#'   for a [Composite_table]
#' @export
#'
#' @examples
#'
#' dat <- data.frame(
#'   apple = 1,
#'   banana = 2,
#'   dog = 1,
#'   cat = 2,
#'   parrot = 3
#' )
#'
#' multinames(dat) <- as_multinames(
#'   c('fruit', 'fruit', 'animal', 'animal', 'animal')
#' )
#'
#' multinames(dat)
#'
as_multinames <- function(x){
  stats::setNames(cumsum(rle(x)[['lengths']]), rle(x)[['values']])
}




#' Flip names and multinames of a Composite Table
#'
#' The column names of the resulting `Composite_table` will be sorted
#' lexically
#'
#' @param dat A `Composite_table`
#' @param id_vars a character vector of column names of `dat`. The selected
#'   columns will not be sorted lexically but kept to the left. If the columns
#'   have a multiname associated with them, they must be supplied in the format
#'   `column_name.multiname`.
#'
#' @return a Composite_table
#' @export
#'
#' @examples
#'
#' dat <- comp_table(
#'   cars1 = head(cars),
#'   cars2 = tail(cars),
#'   data.frame(id = LETTERS[1:6])
#' )
#'
#' flip_names(dat)
#' flip_names(dat, id_vars = "id")
#' flip_names(dat, id_vars = c("id", "speed.cars1"))
#'
flip_names <- function(dat, id_vars){
  UseMethod('flip_names')
}



#' @export
flip_names.Composite_table <- function(
  dat,
  id_vars = NULL
){
  dd <- data.table::as.data.table(dat)
  assert_that(all_are_distinct(names(dd)))
  assert_that(is.null(id_vars) || all(id_vars %in% names(dd)))

  sorted_names <- sort(names(dd)) %>%
    vec_prioritise(id_vars)

  data.table::setcolorder(dd, sorted_names)
  res <- as_Composite_table(dd, reverse = TRUE)
  meta(res) <- meta(dat)

  return(res)
}
