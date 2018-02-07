#* @testfile manual_tests/test_as_lines

#' Create a line-by-line text representation of an R Object
#'
#' Creates a line-by-line representation of an \R Obeject (usually a
#' `Tatoo_table`). This is the function powers all `Tatoo_table` print methods.
#'
#' @template any_r
#' @param color Use colors (via [colt])
#' @param ... passed on methods.
#'
#' @return A character vector (one element per line).
#' @export
#'
as_lines <- function(x, color = TRUE, ...){
  UseMethod("as_lines")
}




#' @rdname as_lines
#' @export
as_lines.data.frame <- function(
  x,
  color = TRUE,
  ...
){
  res <- rbind(
    t(as.matrix(names(x))),
    as.matrix(unname(x))
  )

  res[is.na(res)] <- "NA"
  res <- apply(res, 2, function(x) {
    stringi::stri_pad_left(x, max(crayon::col_nchar(x), na.rm = TRUE))
  })
  res <- apply(res, 1, paste, collapse = " ")

  if(color){
    res[1] <- style_colname(res[1])
  }

  res
}




#' @rdname as_lines
#' @export
as_lines.Tagged_table <- function(
  x,
  color = TRUE,
  ...
){
  dd    <- data.table::copy(x)
  meta  <- attr(dd, 'meta')
  if(!color) style_meta <- identity

  res <- character()

  if(!is.null(meta)){
    res <- c(res, style_meta(make_tag_table_print_title(meta)))
  }

  res <- c(res, NextMethod(as_lines, dd, ...))

  if(!is.null(meta$footer)){
    res <- c(res, style_meta(meta$footer))
  }

  res
}




#' @rdname as_lines
#' @inheritParams mash_table
#' @export
as_lines.Mashed_table <- function(
  x,
  color = TRUE,
  mash_method = attr(x, 'mash_method'),
  insert_blank_row = attr(x, 'insert_blank_row'),
  id_vars = attr(x, 'id_vars'),
  ...
){
  print_multi_headings <-
    identical(mash_method, 'col') &&
    identical(length(names(x)), length(x))


  if(print_multi_headings){
    pdat  <- as_Composite_table(x, meta = NULL)
    lines <- as_lines(pdat, color = color, ...)
  } else {
    lines <- as_lines(
      data.table::as.data.table(
        x,
        mash_method = mash_method,
        insert_blank_row = FALSE,
        id_vars = id_vars
      )
      ,
      color = color
    )
  }


  if(identical(mash_method, 'row')){
    if (insert_blank_row) {
      res <- vector("character", length(lines) + nrow(x[[1]]) - 1)

      i <- j <- 1

      while (i <= length(lines)){
        if ((i - 2)  %% length(x) == 0 && i > 2) {
          j <- j + 1
        }

        res[j] <- lines[i]

        j <- j + 1
        i <- i + 1
      }
    } else {
      res <- lines
      fill_bg <- FALSE

      for(i in seq_along(lines)){
        if ((i > length(x)) && (i - 2) %% length(x) == 0) {
          fill_bg <- !fill_bg
        }

        if(fill_bg && color){
          res[[i]] <- style_bg_subtle(res[[i]])
        }
      }
    }
  } else {
    res <- lines
  }

  res
}




#' @rdname as_lines
#' @export
as_lines.Stacked_table <- function(x, color = TRUE, ...){
  as_lines_several_tables(
    x,
    color = color,
    indent = "`  ",
    sep1 = "`",
    sep2 = "_",
    ...
  )
}




#' @rdname as_lines
#' @export
as_lines.Composite_table <- function(
  x,
  color = TRUE,
  ...
){
  mutlicol_spacing <- "  "

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

    pad_width <- max(crayon::col_nchar(x))
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

    res[[i]] <- do.call(paste, c(dd[sel_cols], sep = mutlicol_spacing))
  }

  tmp <- list()

  for(i in seq_along(res)){
    title  <- stringi::stri_pad_both(
      names(multinames)[[i]], max(crayon::col_nchar(res[[i]])),
      '.'
    )

    column <- stringi::stri_pad_both(
      res[[i]],
      crayon::col_nchar(title)
    )

    tmp[[i]] <- c(title, column)
  }

  res2 <- as.data.frame(tmp, fix.empty.names = FALSE, optional = TRUE) %>%
    as.matrix()

  if(color){
    res2[1, ] <- style_multicolname(res2[1, ])
    res2[2, ] <- style_colname(res2[2, ])
  }

  apply(res2, 1, paste, collapse = mutlicol_spacing)
}




#' @rdname as_lines
#' @export
as_lines.Tatoo_report <- function(x, color = TRUE, ...){
  make_table_heading <- function(y) {
    if ('Tagged_table' %in% class(y)){
      paste(class(y)[1:2], collapse = '> <')
    } else {
      class(y)[[1]]
    }
  }

  classes <- lapply(x, make_table_heading)
  classes <- sprintf('%s <%s>', names(x) %||% '', classes)
  if(color) classes <- style_colname(style_coltypes(classes))

  as_lines_several_tables(
    x,
    color = color,
    indent = "::   ",
    sep1 = 0,
    sep2 = 2,
    headings = classes,
    ...
  )
}




#' @rdname as_lines
#' @export
as_lines.TT_meta <- function(x, color = TRUE, ...){
  name_width   <- max(unlist(lapply(names(x), crayon::col_nchar))) + 1
  print_string <- paste0('%', name_width, 's: %s')
  padded_newline <- rep(' ', name_width + 2) %>%
    paste(collapse = '')

  padded_newline <- paste0('\n', padded_newline)

  res <- vector("list", length(x))

  for(i in seq_along(x)){
    res[[i]] <- sprintf(
      print_string,
      names(x)[[i]], paste(x[[i]], collapse = padded_newline)
    ) %>%
      stringi::stri_split_fixed('\n')
  }


  if(!color) style_meta <- identity
  purrr::map_chr(unlist(res), style_meta)
}




# utils -------------------------------------------------------------------

#' Print several tables
#'
#' Internal function used by `print.Stacked_table()` and
#' `print.Tatoo_report()`
#'
#' @param dat A list of objects that can be printed, usually data.frames
#'   or Tatoo_tables
#' @param indent a scalar character specifying the indent symbols (e.g. `"  "`)
#' @param sep1 character or numeric. Separator above the first and
#'   below the last table.  If character a sep line is created using this
#'   character (i.e. ------). If numeric, that many blank rows are inserted.
#' @param sep2 \code{character} or \code{numeric}. Spacing between the tables.
#'   Like \code{sep1}
#' @param headings \code{character} vector of the same length as \code{dat},
#'   specifying headings to be inserted above each table.
#' @param ... passed on to \code{\link{print}}
#'
#' @noRd
#' @return \code{dat} (invisibly)
#'
as_lines_several_tables <- function(
  dat,
  color,
  indent,
  sep1,
  sep2,
  colors = list(
    indent = style_borders,
    sep1 = style_borders,
    sep2 = style_borders
  ),

  headings = NULL,
  ...
){
  # Preconditions
  assert_that(rlang::is_scalar_character(indent))
  assert_that(
    rlang::is_scalar_character(sep1) ||
    rlang::is_scalar_integerish(sep1)
  )
  assert_that(
    rlang::is_scalar_character(sep2) ||
    rlang::is_scalar_integerish(sep2)
  )
  assert_that(is.null(headings) || identical(length(headings), length(dat)))


  # Process arguments
  tables_char  <- purrr::map(dat, as_lines, color = color)
  tables_width <- max(crayon::col_nchar(unlist(tables_char)))
  sepline1 <- make_sepline(
    sep1, width = tables_width, offset = crayon::col_nchar(indent)
  )
  sepline2 <- make_sepline(sep2, width = tables_width)

  if(color){
    indent   <- colors$indent(indent)
    sepline1 <- colors$sep1(sepline1)
    sepline2 <- colors$sep1(sepline2)
  }


  # Formatting
  if(is.null(headings)){
    res <- purrr::map(
      tables_char,
      function(.x) list(sepline2, paste0(indent, .x))
    )
  } else {
    res <- purrr::map2(
      headings, tables_char,
      function(.x, .y) c(list(sepline2, .x, paste0(indent, .y)))
    )
  }

  res[[1]][[1]] <- NULL  # remove unwanted initial sepline

  res <- unlist(res)
  res <- c(sepline1, res)

  if(sep1 != 0 && sep1 != '') {
    res <- c(res, indent, sepline1)
  }

  res
}
