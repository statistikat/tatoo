#' Title
#'
#' @param dat
#' @param wb
#' @param sheet
#' @param passed on to \code{openxlsx::writeData}
#'
#' @return
#' @export
#'
#' @examples
write_worksheet <- function(
  dat,
  wb,
  sheet,
  append = FALSE,
  start_row = 1L,
  ...
){
  assert_that(requireNamespace("openxlsx"))
  UseMethod('write_worksheet')
}



write_worksheet.default <- function(
  dat,
  wb,
  sheet,
  append = FALSE,
  start_row = 1L,
  ...
){
  openxlsx::writeData(wb = wb, sheet = sheet, x = dat, ...)
}



#' Write worksheet
#'
#' @param dat
#' @param wb
#' @param sheet
#'
#' @return
#' @export
#'
#' @examples
write_worksheet.Pub_table <- function(
  dat,
  wb,
  sheet = sanitize_excel_sheet_names(attr(dat, 'meta')$tableId),
  start_row = 1L
){
  wb %assert_class% 'Workbook'
  meta <- attr(dat, 'meta')
  openxlsx::addWorksheet(wb, sheet)

  cat('ww pub table')


  # Construct header
  crow <- start_row

  title    <- sprintf('%s: %s', meta$tableId, meta$title)
  openxlsx::writeData(wb, sheet = sheet, title)
  crow <- crow + 1

  if(meta$longtitle != meta$title){
    openxlsx::writeData(wb, sheet = sheet, startRow = crow, meta$longtitle)
    crow <- crow + 1
  }

  if (!is.null(meta$subtitle)){
    openxlsx::writeData(wb, sheet = sheet, startRow = crow, meta$subtitle)
    crow <- crow + 1
  }


  # Write Data
  NextMethod(
    generic = write_worksheet,
    object = dat,
    wb = wb,
    sheet = sheet,
    append = TRUE,
    start_row = crow + 1
  )

  return(wb)
}


write_worksheet.Comp_table <- function(
  dat,
  wb,
  sheet,
  append = FALSE,
  start_row = 1L,
  ...
){
  wb %assert_class% 'Workbook'
  cat('ww comp table')

  if(!append){
    openxlsx::addWorksheet(wb, sheet)
  }

  crow <- start_row

  titles <- attr(dat, 'titles')
  assert_that(titles %identical% sort(titles))

  title_row <- vector(mode = 'list', length = ncol(dat))
  title_counter <- 1

  for(i in seq_along(title_row)){
    title_row[[i]] <- names(titles)[[title_counter]]

    if(i %in% titles){
      title_counter <- title_counter + 1
    }
  }

  # Write super-headings
  openxlsx::writeData(
    wb,
    sheet = sheet,
    as.data.frame(title_row),
    colNames = FALSE,
    startRow = crow
  )

  crow <- crow + 1


  for(i in seq_along(titles)){
    merge_start <- ifelse(i == 1L, 1, titles[[i-1]] + 1)
    merge_end   <- titles[[i]]
    openxlsx::mergeCells(
      wb,
      cols = c(merge_start, merge_end),
      rows = 1,
      sheet = sheet
    )
  }

  openxlsx::writeData(
    wb,
    sheet = sheet,
    startRow = crow,
    dat,
    colNames = TRUE
  )

  return(wb)
}






sanitize_excel_sheet_names <- function(x){
  invalid_chars_regex <- "\\[|\\]|\\*|\\?|:|\\/|\\\\"
  res <- stringi::stri_replace_all_regex(x, invalid_chars_regex,'_')
  stringi::stri_sub(res, 1, 31)
}
