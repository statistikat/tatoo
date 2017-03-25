#' `save_xlsx` is a shortcut to save a Tatoo_table direclty to local .xlsx
#' file.
#'
#' @param outfile path/name of the output file
#' @param overwrite logical. If \code{TRUE}, overwrite any existing file.
#'
#' @md
#' @return TRUE on success
#' @export
#' @rdname as_workbook
#'
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
  invisible(wb)
}
