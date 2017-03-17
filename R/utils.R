# Utils -------------------------------------------------------------------

#' Sanitze excel sheet names
#'
#' Convert a vactor to valid excel sheet names by:
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
#' @md
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
  assert_that(purrr::is_vector(x))
  assert_that(purrr::is_scalar_character(replace))
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
  assert_that(hammr::all_unique(res, silent = TRUE))
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
#' Internal function used by \code{print.Stacked_table} and
#' \code{print.Tatoo_report}
#'
#' @param dat A \code{list} of objects that can be printed, usually \code{data.frame}s
#'   or \code{Tatoo_table}s
#' @param indent a scalar character specifing the indent symbols (e.g. "  ")
#' @param sep1 \code{character} or \code{numeric}. Seperator above the first and
#'   below the last table.  If character a sep line is created using this
#'   character (i.e. ------). If numeric, that many blank rows are inserted.
#' @param sep2 \code{character} or \code{numeric}. Spacing between the tables.
#'   Like \code{sep1}
#' @param headings \code{character} vector of the same length as \code{dat},
#'   specifiying headings to be inserted above each table.
#' @param ... passed on to \code{\link{print}}
#'
#' @return \code{dat} (invisibly)
#'
print_several_tables <- function(
  dat,
  indent,
  sep1,
  sep2,
  headings = NULL,
  ...
){
  assert_that(is.null(headings) || identical(length(headings), length(dat)))

  tables_char <- dat %>%
    lapply(function(x) capture.output(print(x, ...)))

  # Get width for print output
  tables_width <- tables_char %>%
    purrr::map(function(x) purrr::map_int(x, nchar)) %>%
    unlist() %>%
    max()
  assert_that(purrr::is_scalar_integer(tables_width))


  # Define sepperators
  make_sepline <- function(x){
    if(is.character(x)){
      sepline1 <- paste(rep(x, tables_width + nchar(indent) + 1), collapse = '')
    } else if (is.numeric(x)){
      sepline1 <- paste0(paste0(rep('\n', x), collapse = ''))
    } else {
      stop('Sep must be either character or an integer number (for number of blank lines to insert)')
    }
  }

  sepline1 <- make_sepline(sep1)
  sepline2 <- make_sepline(sep2)

  cat('', sepline1, '\n')
  for(i in seq_along(tables_char)){
    if(!is.null(headings)){
      cat(headings[[i]])
    }
    lapply(tables_char[[i]], function(x) cat(indent, x, '\n'))
    if(i < length(tables_char)){
      if(is.numeric(sep2)){
        cat(sepline2)
      } else {
        cat(indent, sepline2, '\n')
      }
    }
  }

  if(sep1 != 0 && sep1 != '') {
    cat(indent, '\n', sepline1, '\n')
  }

  invisible(dat)
}
