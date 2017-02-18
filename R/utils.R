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
