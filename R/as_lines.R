#' Title
#'
#' @param x
#' @param color
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
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


  res <- apply(res, 2, function(x) stringi::stri_pad_left(x, max(nchar(x))))
  res <- apply(res, 1, paste, collapse = " ")

  if(color){
    res[1] <- style_colname(res[1])
  }

  res
}




#' @rdname as_lines
#' @export
as_lines.Tagged_table <- function(x, ...){
  dd    <- data.table::copy(x)
  meta  <- attr(dd, 'meta')

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
#' @export
as_lines.Mashed_table <- function(
  x,
  mash_method = attr(x, 'mash_method'),
  insert_blank_row = attr(x, 'insert_blank_row'),
  id_vars = attr(x, 'id_vars'),
  color = TRUE,
  ...
){
  print_multi_headings <-
    identical(mash_method, 'col') &&
    identical(length(names(x)), length(x))


  if(print_multi_headings){
    pdat  <- as_Composite_table(x, meta = NULL)
    lines <- as_lines(pdat, ...)
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

        if(fill_bg){
          res[[i]] <- colt::clt_bg_subtle(res[[i]])
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
as_lines.Stacked_table <- function(x, ...){
  as_lines_several_tables(
    x,
    indent = "`  ",
    sep1 = "`",
    sep2 = "_",
    ...
  )
}




#' @rdname as_lines
#' @export
as_lines.Composite_table <- function(x, right = FALSE, color = TRUE, ...){
  mutlicol_spacing = "  "

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

    res[[i]] <- do.call(paste, c(dd[sel_cols], sep = mutlicol_spacing))
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

    tmp[[i]] <- c(title, column)
  }

  res2 <- as.data.frame(tmp, fix.empty.names = FALSE, optional = TRUE) %>%
    as.matrix()

  if(color){
    res2[1, ] <- style_multicolname(res2[1, ])
    res2[2, ] <- style_colname(res2[2, ])
  }

  apply(res2, 1 , paste, collapse = mutlicol_spacing)
}




#' @rdname as_lines
#' @export
as_lines.Tatoo_report <- function(x, ...){
  make_table_heading <- function(y) {
    if ('Tagged_table' %in% class(y)){
      paste(class(y)[1:2], collapse = '> <')
    } else {
      class(y)[[1]]
    }
  }

  classes <- lapply(x, make_table_heading)
  classes <- sprintf('%s <%s>', names(x) %||% '', classes)
  classes <- style_coltypes(classes)

  as_lines_several_tables(
    x,
    indent = "::   ",
    sep1 = 0,
    sep2 = 2,
    headings = style_colname(classes),
    ...
  )
}




#' @rdname as_lines
#' @export
as_lines.TT_meta <- function(x){
  name_width   <- max(unlist(lapply(names(x), nchar))) + 1
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

  purrr::map_chr(unlist(res), style_meta)
}
