# Utils -------------------------------------------------------------------

#' Sanitze excel sheet names
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples
sanitize_excel_sheet_names <- function(x){
  invalid_chars_regex <- "\\[|\\]|\\*|\\?|:|\\/|\\\\"
  res <- stringi::stri_replace_all_regex(x, invalid_chars_regex, '_')
  res <- stringi::stri_sub(res, 1, 31)

  for(el in unique(res)){
    suffix <- cumsum(duplicated(res[res == el]))

    if(length(res[res == el]) > 1L){
      res[res == el] <- paste0(
        strtrim(res[res == el], 31 - max(nchar(suffix))),
        suffix)
    }
  }

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



print_several_tables <- function(dat, indent, sep1, sep2, headings = NULL){
  assert_that(is.null(headings) || identical(length(headings), length(dat)))

  tables_char <- dat %>%
    lapply(function(x) capture.output(print(x)))

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
}
