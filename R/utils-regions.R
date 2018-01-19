excel_range_to_indices <- function(x){
  splt <- stringi::stri_split_fixed(x, ":")

  lapply(
    splt,
    function(y){
      col <- as.integer(openxlsx::convertFromExcelRef(stringi::stri_extract_first_regex(y, "^[A-Z]*")))
      row <- as.integer(stringi::stri_extract_first_regex(y, "\\d*$"))


      expand.grid(
        rows = seq(row[[1]], row[[2]]),
        cols = seq(col[[1]], col[[2]])
      )
    }
  )
}




#' Title
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples
excel_regions_to_dt <- function(x){
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
