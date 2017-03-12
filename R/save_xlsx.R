#' As workbook
#'
#' @param dat
#' @param outfile
#' @param overwrite
#'
#' @return
#' @export
#'
#' @examples
save_xlsx <- function(dat, outfile, overwrite, ...){
  UseMethod('save_xlsx')
}



#' @export
save_xlsx.default <- function(
  dat,
  outfile,
  overwrite = FALSE,
  ...
){
  wb <- as_workbook(dat, ...)
  openxlsx::saveWorkbook(wb, outfile, overwrite)
}




#' Title
#'
#' @param dat
#' @param outfile
#' @param mash_method
#' @param insert_blank_row
#' @param sep_height
#' @param sheet_name
#' @param overwrite
#' @param ... parameters passed on to
#'
#' @return
#' @export
#'
#' @examples
save_xlsx.Mashed_table <- function(
  dat,
  outfile,
  mash_method = 'row',
  insert_blank_row = FALSE,
  sep_height = 24,
  overwrite = FALSE
){
  wb <- as_workbook(
    dat,
    mash_method = mash_method,
    insert_blank_row = insert_blank_row,
    sep_height = sep_height
  )

  openxlsx::saveWorkbook(wb, outfile, overwrite = overwrite)
}
