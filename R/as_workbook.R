#' As Workbook
#'
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
as_workbook <- function(
  dat,
  sheet = 1L,
  ...
){
  assert_that(requireNamespace("openxlsx"))
  assert_that(is.scalar(sheet))

  UseMethod('as_workbook')
}


#' @export
as_workbook.default <- function(
  dat,
  sheet = 1,
  ...
){
  wb <- openxlsx::createWorkbook()
  wb <- write_worksheet(dat, wb, sheet = sheet, ...)
  return(wb)
}



#' @export
as_workbook.Pub_table <- function(
  dat,
  sheet = attr(dat, 'meta')$table_id,
  ...
){
  wb <- openxlsx::createWorkbook()
  wb <- write_worksheet(dat, wb, sheet = sheet, ...)
  return(wb)
}



#' @export
as_workbook.Mash_table <- function(
  dat,
  sheet = 1L,
  mash_method = 'row',
  insert_blank_row = TRUE,
  sep_height = 20
){
  assert_that(is.scalar(mash_method))
  assert_that(is.flag(insert_blank_row))
  assert_that(is.number(sep_height))

  wb <- openxlsx::createWorkbook()
  wb <- write_worksheet(
    dat,
    wb,
    sheet = sheet,
    append = FALSE,
    mash_method = mash_method,
    insert_blank_row = insert_blank_row,
    sep_height = sep_height
  )

  return(wb)
}




#' Convert Pub_report to openxlsx Workbook
#'
#' Converts a Pub_report to a an \code{openxlsx::Workbook} object. Sheet names
#' will be taken from the element names of Pub_report
#' (\code{names(yourPub_report)}).
#'
#' @param dat a pub report
#'
#' @export
as_workbook.Pub_report <- function(dat){
  wb <- openxlsx::createWorkbook()

  for(i in seq_along(dat)){
    if(is.null(names(dat))){
      sheet_name <- i
    } else {
      sheet_name <- sanitize_excel_sheet_names(names(dat))[[i]]
    }

    wb <- write_worksheet(
      dat = dat[[i]],
      wb = wb,
      sheet = sheet_name,
      append = FALSE,
      start_row = 1L
    )

    wb %assert_class% 'Workbook'
  }

  return(wb)
}
