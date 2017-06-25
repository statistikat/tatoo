# Ctors -------------------------------------------------------------------

#' Compose Tables
#'
#' @param ... `comp_table` only: individual data.frames. A name can be
#'   provided for each data.frame that will be used by [print] and
#'   [as_workbook()] to create multi-table headings.
#' @param id_vars If id_vars is specified, the tables will be combined
#'   using [merge()] on the columns specified in id_vars, otherwise
#'   the tables will be combined with [cbind()].
#' @param meta a [TT_meta] object. If speciefied, the resulting
#'   Composite_table will be wrapped in a [Tagged_table].
#'
#' @return a Composite_table
#'
#' @md
#' @rdname Composite_table
#' @aliases comp_table composite_table Composite_table
#' @family Tatto tables
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
      purrr::map(names) %>%
      purrr::map_lgl(function (x) id_vars %in% x)

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
  assert_that(all(looks_like_integer(multinames)))

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
#' Converts other R objects to Composite Tables by automatically creating
#' multi-column names from the properties of the objects.
#'
#'
#' @param dat any R object
#' @param ... ignored
#'
#' @return
#' `as_Composte_table()` returns a Composite_table
#'
#' `is_Composite_table()` returns `TRUE` if its argument is a Composite_table
#' and `FALSE` otherwise.
#'
#' @md
#' @export
as_Composite_table <- function(dat, ...){
  UseMethod('as_Composite_table')
}




#' `as_Composite_table.Mashed_table()` extracts the multi-column names from the
#'   column names of the individual data.frames that make up a [Mashed_table],
#'   and the column names from the names of the Mashed Table.
#'
#' @inheritParams comp_table
#'
#' @md
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
  dat,
  id_vars = attr(dat, 'id_vars'),
  meta = attr(dat, 'meta'),
  ...
){
  assert_that(length(names(dat)) %identical% length(dat))
  assert_that(is.null(id_vars) || all(id_vars %in% names(dat[[1]])))

  col_names <- names(dat[[1]])[! names(dat[[1]]) %in% id_vars]
  n_idv  <- length(id_vars)
  n_tbls <- length(dat)
  n_cols <- ncol(dat[[1]]) - n_idv

  if(n_idv > 0){
    multinames        <- cumsum(c(n_idv, rep(n_tbls, n_cols)))
    names(multinames) <- c('', col_names)
  } else {
    multinames        <- cumsum(c(rep(n_tbls, n_cols)))
    names(multinames) <- names(dat[[1]])
  }

  res <- data.table::as.data.table(dat, mash_method = 'col', id_vars = id_vars)
  names(res) <- c(id_vars, rep(names(dat), n_cols))
  multinames(res) <- multinames
  meta(res) <- meta

  return(res)
}




#' `as_Composite_table.data.frame()` extracts the mutli-column names from the
#'   column names of a `data.frame` based on a sepparator.
#'
#' @param sep a scalar character. Sepparator in the column names of `dat` that
#'   separates the column name from the multi-column name.
#' @param reverse logical. if `FALSE` the part after the last occurence of `sep`
#'   will be used as multiname, if `TRUE` the part before will be used.
#'
#' @export
#' @rdname as_Composite_table
#' @md
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
  dat,
  sep = ".",
  reverse = FALSE,
  ...
){
  as_Composite_table(
    as.data.table(dat),
    sep = sep,
    reverse = reverse
  )
}




#' @export
as_Composite_table.data.table <- function(
  dat,
  sep = ".",
  reverse = FALSE,
  ...
){
  assert_that(purrr::is_scalar_character(sep))
  assert_that(is.flag(reverse))

  # Process inputs
    res <- data.table::copy(dat)


  # Logic
    splt_pos <- stringi::stri_locate_last_fixed(names(dat), sep)[, 1]
    cnames <- stringi::stri_sub(names(dat), to = splt_pos - 1)
    mnames <- stringi::stri_sub(names(dat), from = splt_pos + 1)

    if(reverse){
      tmp <- cnames
      cnames <- mnames
      mnames <- tmp
    }

    sel <- is.na(cnames)
    cnames[sel] <- names(dat)[sel]
    mnames[is.na(mnames)] <- ''

  res %>%
    data.table::setnames(cnames) %>%
    Composite_table(as_multinames(mnames))
}




#' @rdname as_Composite_table
#' @export
is_Composite_table <- function(dat, ...){
  inherits(dat, 'Composite_table')
}




# Methods -----------------------------------------------------------------

#' Printing Composite Tables
#'
#' @param x a \code{Tagged_table}
#' @param right \code{logical}, indicating whether or not strings should be
#'   right-aligned. The default is left-alignment (the opposite of the
#'   standard \code{print.data.frame} method.
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
  if(!has_attr(x, 'multinames')){
    warning(
      'x is not a valid composite table: no multinames attribute found.',
      call. = FALSE
    )
    print(as.data.table(x, multinames = FALSE))
    return(invisible())
  }

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

    dd <- vector('list', ncol(x))

    for(i in seq_along(dd)){
      dd[[i]] <- prep_col(x[[i]], names(x)[[i]])
    }


  # Merge columns that belong to the same title
    multinames <- attr(x, 'multinames')
    res <- vector('list', length(multinames))
    names(res) <- names(multinames)

    for(i in seq_along(multinames)){
      res[[i]] <- multinames[(i - 1):i]

      if(identical(i, 1L)){
        sel_cols <- 1:multinames[[i]]
      } else {
        sel_cols <- (multinames[i - 1] + 1):multinames[i]
      }

      res[[i]] <- do.call(paste, c(dd[sel_cols], sep = "   "))
    }

    tmp <- list()

    for(i in seq_along(res)){
      title  <- stringi::stri_pad_both(
        names(multinames)[[i]], max(nchar(res[[i]])),
        '.'
      )

      column <- stringi::stri_pad_both(
        res[[i]],
        nchar(title)
      )

      sep    <- rep('  ', length(res[[i]]))

      tmp[[i]] <- list(column, sep)
      names(tmp[[i]]) <- c(title, '   ')
    }

    res2 <- unlist(tmp, recursive = FALSE)
    res2 <- as.data.frame(res2, fix.empty.names = FALSE, optional = TRUE)

    print(res2, right = right, ...)

    invisible(x)
}




#' Convert a Composite Table to a plain data.table or data.frame
#'
#' As a \code{Composite_table} already is a \code{data.table} this function
#' does very little except stripping all additional attributes and classes,
#' as well as offering you the option to prepend the \code{multinames} before
#' the column names
#'
#' @param x a \code{Composite_table}
#' @param multinames logical. Whether to prepend multinames before the column
#'   names
#' @param sep sepparator between multinames and individual column names
#' @param ... ignored
#'
#' @return a \code{data.table} or \code{data.frame}
#'
#' @method as.data.table Composite_table
#' @export
as.data.table.Composite_table <- function(
  x,
  multinames = TRUE,
  sep = '.',
  ...
){
  # Preconditions
    assert_that(is.flag(multinames))
    assert_that(purrr::is_scalar_character(sep))


  # Process arguments
    x <- data.table::copy(x)


  # Logic
    if(!multinames){
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
  multinames = TRUE,
  sep = '.',
  ...
){
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
#' @param dat a Composite_table or data.frame
#' @param value a named vector of ascending integers. The name is the
#'   multi-column heading, the integer value is the last colum that this
#'   heading applies to
#'
#' @md
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
#' multinames(df_mean) = c('species' = 1, measures = 3)
#'
#' # .species..     ...measures...
#' # 1    Species     length   width
#' # 2     setosa       5.01    3.43
#' # 3 versicolor       5.94    2.77
#' # 4  virginica       6.59    2.97
#'
#'
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




#' @rdname multinames
#' @export
multinames <- function(dat){
  assert_that(inherits(dat, 'Composite_table'))
  attr(dat, 'multinames')
}




# Utils -------------------------------------------------------------------

composite_name <- function(x, y, sep){
  if(x == ''){
    return(y)
  } else {
    paste(x, y, sep = sep)
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
#' @md
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
