#' As Workbook
#'
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
as_workbook <- function(...){
  assert_that(requireNamespace("openxlsx"))
  UseMethod('as_workbook')
}


as_workbook.default <- function(dat){
  wb <- openxlsx::createWorkbook()
  wb <- write_worksheet(dat, wb, sheet = 1)
  return(wb)
}

#' As Workbook
#'
#' @param dat
#' @param stack_method
#' @param insert_blank_row
#' @param sep_height
#' @param sheet_name
#' @param ... parameters passed on to openxlsx::writeDate
#'
#' @return
#' @export
#'
#' @examples
as_workbook.Stack_table <- function(dat,
                                    stack_method,
                                    insert_blank_row,
                                    sep_height = 20,
                                    sheet_name = 'sheet1',
                                    ...){

  res <- as.data.table(dat,
                       stack_method = stack_method,
                       insert_blank_row = insert_blank_row)

  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, '')
  openxlsx::writeData(wb, 1, res, ...)


  if(!is.null(sep_height)){
    if('startRow' %in% names(list(...))){
      row_off <- list(...)[['startRow']] - 1
    } else if ('xy' %in% names(list(...))){
      row_off <- list(...)[['xy']][[2]] - 1
    } else {
      row_off <- 0
    }

    if(insert_blank_row){
      sel_rows <- seq(4 + row_off, nrow(res) + row_off, by = 3)
    } else {
      sel_rows <- seq(4 + row_off, nrow(res) + row_off, by = 2)
    }

    openxlsx::setRowHeights(wb, 1, sel_rows, sep_height)
  }

  return(wb)
}


#' Title
#'
#' @param dat
#' @param outfile
#' @param stack_method
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
save_xlsx.StackTable <- function(dat,
                                 outfile,
                                 stack_method = 'row',
                                 insert_blank_row = FALSE,
                                 sep_height = 24,
                                 sheet_name = 'sheet1',
                                 overwrite = FALSE,
                                 ...){

  wb <- as_workbook(dat,
                    stack_method = stack_method,
                    insert_blank_row = insert_blank_row,
                    sep_height = sep_height,
                    sheet_name = sheet_name,
                    ...)

  openxlsx::saveWorkbook(wb, outfile, overwrite = overwrite)
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
    ptable <- dat[[i]]

    if(is.null(names(dat))){
      sheet_name = i
    } else {
      sheet_name = sanitize_excel_sheet_names(names(dat))[[i]]
    }

    wb <- write_worksheet(dat[[i]], wb, sheet = sheet_name)
  }

  return(wb)
}



