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
  sheet = sanitize_excel_sheet_names(attr(dat, 'meta')$table_id),
  start_row = 1L
){
  wb %assert_class% 'Workbook'
  meta <- attr(dat, 'meta')
  openxlsx::addWorksheet(wb, sheet)
  crow   <- start_row

  # Construct header
    header <- list()

    header$title <- sprintf('%s: %s', meta$table_id, meta$title)

    if(meta$longtitle != meta$title){
      header$longtitle <- meta$longtitle
    }

    if (!is.null(meta$subtitle)){
      header$subtitle <- meta$subtitle
    }

    header <- t(as.data.frame(header))

    openxlsx::writeData(
      wb,
      sheet = sheet,
      header,
      rowNames = FALSE,
      colNames = FALSE
    )

    crow <- crow + nrow(header) + 1

  # Write Data
    NextMethod(
      generic = write_worksheet,
      object = dat,
      wb = wb,
      sheet = sheet,
      append = TRUE,
      start_row = crow
    )


  # Write Footer
    if (!is.null(meta$footer)){
      crow <- openxlsx::readWorkbook(
        wb,
        sheet = sheet,
        colNames = FALSE,
        skipEmptyRows = FALSE
      ) %>%
        nrow()

      crow <- crow + 3
      openxlsx::writeData(wb, sheet = sheet, startRow = crow, meta$footer)
    }

  return(wb)
}


#' Title
#'
#' @param dat
#' @param wb
#' @param sheet
#' @param append
#' @param start_row
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
write_worksheet.Comp_table <- function(
  dat,
  wb,
  sheet,
  append = FALSE,
  start_row = 1L,
  ...
){
  wb %assert_class% 'Workbook'

  if(!append){
    openxlsx::addWorksheet(wb, sheet)
  }

  crow   <- start_row
  titles <- attr(dat, 'titles')

  assert_that(titles %identical% sort(titles))

  title_row     <- vector(mode = 'list', length = ncol(dat))
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
      rows = start_row,
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




