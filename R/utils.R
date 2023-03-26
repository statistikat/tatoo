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
  assert_that(
    is_vector(x),
    is_scalar_character(replace)
  )
  x <- as.character(x)

  invalid_chars_regex <- "\\[|\\]|\\*|\\?|:|\\/|\\\\"
  res <- stringi::stri_replace_all_regex(x, invalid_chars_regex, replace)
  res <- stringi::stri_sub(res, 1, 31)

  for(el in unique(res)){
    suffix <- cumsum(duplicated(res[res == el]))

    if(length(res[res == el]) > 1L){
      res[res == el] <- paste0(
        strtrim(res[res == el], 31 - max(crayon::col_nchar(suffix))),
        suffix)
    }
  }

  assert_that(
    is.character(res),
    all_are_distinct(res)
  )
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
    stop("OS not supported")
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
  assert_that(requireNamespace("knitr", quietly = TRUE))
}




require_openxlsx <- function(){
  assert_that(requireNamespace("openxlsx", quietly = TRUE))
}




strip_newlines <- function(x){
  gsub("\r?\n|\r", " ", x)
}




assert_rownames_is_null <- function(x){
  if (!is.null(x)){
    stop("Detected non-NULL keep.rownames argument. Please name all arguments explicitely.")
  }

  TRUE
}
