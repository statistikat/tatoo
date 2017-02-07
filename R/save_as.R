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



#' @export
save_xlsx.Pub_table <- function(
  dat,
  outfile,
  overwrite = FALSE,
  ...
){
  wb <- as_workbook(dat, ...)
  openxlsx::saveWorkbook(wb, outfile, overwrite)
}
