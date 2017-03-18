#' Save a Tatoo object directly to an xlsx file
#'
#' Convenience function to direclty save a tatoo object to an xlsx file.
#'
#' @param dat A \code{\link{Tatoo_table}} or \code{\link{Tatoo_report}}
#' @param outfile path/name of the output file
#' @param overwrite logical. If \code{TRUE}, overwrite any existing file.
#' @param ... passed onto \code{\link{write_worksheet}}
#'
#' @return
#' @export
#'
#' @examples
save_xlsx <- function(
  dat,
  outfile,
  overwrite = FALSE,
  ...
){
  assert_that(purrr::is_scalar_character(outfile))
  assert_that(is.flag(overwrite))

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
