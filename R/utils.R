# Utils -------------------------------------------------------------------

#' Sanitize excel sheet names
#'
#' Convert a vector to valid excel sheet names by:
#' * trimming names down to 31 characters,
#' * ensuring each element of the vector is unique,
#' * and removing the illegal characters \code{ \ / * [ ] : ?}.
#'
#' @param x a vector (or anything that can be coerced to one via
#'   \code{as.character()}).
#' @param replace a scalar character to replace illegal characters with
#'
#' @return a character vector of valid excel sheet names
#' @export
#'
#' @examples
#'
#' sanitize_excel_sheet_names(
#'   c("a very: long : vector? containing some illegal characters",
#'     "a very: long : vector? containing some illegal characters")
#' )
#'
#'   # [1] "a very_ long  vector_ containi0" "a very_ long  vector_ containi1"
sanitize_excel_sheet_names <- function(x, replace = '_'){
  assert_that(rlang::is_vector(x))
  assert_that(rlang::is_scalar_character(replace))
  x <- as.character(x)

  invalid_chars_regex <- "\\[|\\]|\\*|\\?|:|\\/|\\\\"
  res <- stringi::stri_replace_all_regex(x, invalid_chars_regex, replace)
  res <- stringi::stri_sub(res, 1, 31)

  for(el in unique(res)){
    suffix <- cumsum(duplicated(res[res == el]))

    if(length(res[res == el]) > 1L){
      res[res == el] <- paste0(
        strtrim(res[res == el], 31 - max(nchar(suffix))),
        suffix)
    }
  }

  assert_that(is.character(res))
  assert_that(all_are_distinct(res, silent = TRUE))
  return(res)
}




get_final_wb_row <- function(wb, sheet){
  i_sheet <- match(sheet, wb$sheet_names)
  if(wb$worksheets[[i_sheet]]$sheet_data$rows %identical% integer()){
    return(0L)
  } else {
    return(max(wb$worksheets[[i_sheet]]$sheet_data$rows))
  }
}




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
#' @return \code{dat} (invisibly)
#'
as_lines_several_tables <- function(
  dat,
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
    tables_char  <- purrr::map(dat, as_lines)
    tables_width <- max(nchar(unlist(tables_char)))
    indent <- colors$indent(indent)
    sepline1 <- colors$sep1(make_sepline(sep1, width = tables_width, offset = nchar(indent)))  #nolint
    sepline2 <- colors$sep2(make_sepline(sep2, width = tables_width))


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




get_dot_names <- function(...){
  sapply(as.list(substitute(list(...)))[-1L], deparse)
}




print_lines <- function(x){
  for (i in x) cat(i, "\n")
}



make_sepline <- function(x, width, offset = 0){
  if(is.character(x)){
    res <- paste(rep(x, width + offset), collapse = '')
  } else if (is.numeric(x)){
    res <- rep('', x)
  } else {
    stop("Sep must be either character or an integer number")
  }
  return(res)
}
