# Utils -------------------------------------------------------------------

#' Sanitize excel sheet names
#'
#' Convert a vector to valid excel sheet names by:
#' * trimming names down to 31 characters,
#' * ensuring each element of the vector is unique, and
#' * removing the illegal characters `\ / * [ ] : ?`
#'
#' @param x a vector (or anything that can be coerced to one via
#'   [as.character()]).
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




#' Open a file
#'
#' Open a file with the default associated program. Might behave differently
#' depending on the operating system.
#'
#' @param x `character` scalar. Path to the file to open.
#'
#' @return `NULL` (invisibly)
#' @export
#'
open_file <- function(x){
  os <- Sys.info()[["sysname"]]

  if (os == "Windows") {
    cmd <- sprintf('start "" "%s"', x)
    shell(cmd)
  } else if (os == "Linux") {
    system2("xdg-open", x)
  } else {
    system2("open", x)  # MacOS?
  }

  invisible()
}




get_final_wb_row <- function(wb, sheet){
  i_sheet <- match(sheet, wb$sheet_names)
  if(wb$worksheets[[i_sheet]]$sheet_data$rows %identical% integer()){
    return(0L)
  } else {
    return(max(wb$worksheets[[i_sheet]]$sheet_data$rows))
  }
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




require_knitr <- function(){
  requireNamespace("knitr", quietly = TRUE)
}
