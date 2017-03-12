#' Convert an object to a workbook
#'
#' see \code{methods(as_workbook)} and the closely related
#' \code{methods(write_worksheet)}
#'
#' @param dat see \code{methods(as_workbook)} and the related
#'   \code{methods(write_worksheet)}
#' @param sheet The worksheet to write to. Can be the worksheet index or name.
#' @param ... passed on to \code{write_worksheet}
#'
#' @return an openxlsx \code{\link[openxlsx]{Workbook}} object
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




#' @rdname as_workbook
#' @export
as_workbook.default <- function(
  dat,
  sheet = 1,
  ...
){
  wb <- openxlsx::createWorkbook()
  wb <- write_worksheet(
    dat = dat,
    wb = wb,
    sheet = sheet,
    ...
  )
  return(wb)
}



#' Convert a Meta_table to an openxlsx Workbook
#'
#' @export
#' @rdname as_workbook
as_workbook.Meta_table <- function(
  dat,
  sheet = attr(dat, 'meta')$table_id %||% 1L,
  ...
){
  print(sheet)

  wb <- openxlsx::createWorkbook()
  wb <- write_worksheet(dat, wb, sheet = sheet, ...)
  return(wb)
}



#' @param mash_method either \code{row] or \code{col}
#' @param insert_blank_row Only for \code{\link{Mash_table}} and only when
#'   mash_method is 'row': logical. Insert a blank row between row-pairs
#' @param sep_height Only for \code{\link{Mash_table}}: if
#'   \code{insert_blank_row} is \code{FALSE}, the row height of the first row of
#'   each row-pair, if \code{insert_blank_row} is \code{TRUE} the row height of
#'   the blank row.
#'
#' @rdname as_workbook
#' @export
as_workbook.Mash_table <- function(
  dat,
  sheet = 1L,
  mash_method = 'row',
  insert_blank_row = TRUE,
  sep_height = 20,
  ...
){
  assert_that(is.scalar(mash_method))
  assert_that(is.flag(insert_blank_row))
  assert_that(is.number(sep_height))

  wb <- openxlsx::createWorkbook()
  wb <- write_worksheet(
    dat = dat,
    wb = wb,
    sheet = sheet,
    append = FALSE,
    mash_method = mash_method,
    insert_blank_row = insert_blank_row,
    sep_height = sep_height
  )

  return(wb)
}





#' When converting a TT_report, which is a named \code{list} with an additional
#' class attribute, sheet names of the resulting \code{Workbook} will be taken
#' from the element names of TT_report.
#'
#' @rdname as_workbook
#' @export
as_workbook.TT_report <- function(dat, ...){
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
      start_row = 1L,
      ...
    )

    wb %assert_class% 'Workbook'
  }

  return(wb)
}
