#' Get Named Regions of an Excel Sheet as Data.Table
#'
#' @param x An openxlsx workbook or a `character` vector with attributes
#'   `position` and `sheet` as returned by [openxlsx::getNamedRegions()]
#'
#' @return A `data.table`
#' @export
#'
excel_regions_to_dt <- function(x) {
  UseMethod("excel_regions_to_dt")
}




excel_regions_to_dt.character <- function(x){
  msg <- paste(
    "Expecting a character vector with attributes 'position' and 'sheet'",
    "as returned by openxlsx::getNamedRegions()"
  )
  assert_that(has_attr(x, "position"), msg = msg)
  assert_that(has_attr(x, "sheet"), msg = msg)

  indices <- excel_range_to_indices(attr(x, "position"))
  res     <- vector("list", length(x))

  for (i in seq_along(x)){
    res[[i]] <- data.table(
      region  = x[[i]],
      sheet   = attr(x, "sheet")[[i]],
      rows    = indices[[i]]$rows,
      cols    = indices[[i]]$cols
    )
  }

  data.table::rbindlist(res)
}



#' @export
excel_regions_to_dt.Workbook <- function(x){
  excel_regions_to_dt.character(openxlsx::getNamedRegions(x))
}




excel_range_to_indices <- function(x){
  splt <- stringi::stri_split_fixed(x, ":")

  lapply(
    splt,
    function(y){
      col <-
        as.integer(
        openxlsx::convertFromExcelRef(
        stringi::stri_extract_first_regex(y, "^[A-Z]*")
      ))
      row <- as.integer(stringi::stri_extract_first_regex(y, "\\d*$"))
      expand.grid(
        rows = seq(row[[1]], row[[2]]),
        cols = seq(col[[1]], col[[2]])
      )
    }
  )
}
