print_tex <- function(x, ...){
  UseMethod("print_tex")
}



print_tex.Mashed_table <- function(
  x,
  mash_method = attr(x, 'mash_method'),
  id_vars  = attr(x, 'id_vars'),
  insert_blank_row = attr(x, 'insert_blank_row'),
  sep_height = attr(x, 'sep_height'),
  font_size = 12,
  ...
){
  require_knitr()

  sep_height = sep_height - 12 * !insert_blank_row

  x %>%
    as.data.table(id_vars = id_vars) %>%
    knitr::kable(
      format = "latex",
      booktabs = TRUE,
      linesep = c(rep('', length(x) - 1), sprintf("\\addlinespace[%spt]", sep_height))
    )
}


#' Save Table as PDF
#'
#' @param x
#' @param outfile
#' @param mash_method
#' @param id_vars
#' @param insert_blank_row
#' @param overwrite
#' @param template
#' @param papersize `character` scalar. Passed on to the latex command
#'   `\\geometry` from the 'geometry' package. Valid values are: `a0paper,
#'   a1paper, a2paper, a3paper, a4paper, a5paper, a6paper, b0paper, b1paper,
#'   b2paper, b3paper, b4paper, b5paper, b6paper, c0paper, c1paper, c2paper,
#'   c3paper, c4paper, c5paper, c6paper, b0j, b1j, b2j, b3j, b4j, b5j, b6j,
#'   ansiapaper, ansibpaper, ansicpaper, ansidpaper, ansiepaper, letterpaper,
#'   executivepaper, legalpaper`
#' @param orientation `character` scalar. Passed on to the latex command
#'   `\\geometry` from the 'geometry' package. Valid values are:
#'   `portrait`, `landscape`
#' @param keep_source
#'
#' @return
#' @export
#'
#' @examples
save_pdf <- function(
  x,
  ...,
  papersize = "a4paper",
  orientation = "portrait",
  keep_source = FALSE,
  template = system.file("templates", "save_tex.Rmd", package = "tatoo")
){
  UseMethod("save_pdf")
}



save_pdf.Mashed_table <- function(
  x,
  outfile,
  mash_method = attr(x, 'mash_method'),
  id_vars  = attr(x, 'id_vars'),
  insert_blank_row = attr(x, 'insert_blank_row'),
  overwrite = FALSE,
  papersize = "a4paper",
  orientation = "portrait",
  keep_source = FALSE,
  template = system.file("templates", "save_tex.Rmd", package = "tatoo")
){
  assert_that(rlang::is_scalar_character(papersize))

  temp <- readLines(template)

  tex <- print_tex(x, insert_blank_row = insert_blank_row)
  tex <- gsub("{TABLE}", tex, temp, fixed = TRUE)
  tex <- gsub("{PAPERSIZE}", papersize, tex, fixed = TRUE)
  tex <- gsub("{ORIENTATION}", orientation, tex, fixed = TRUE)
  tex <- paste(tex, collapse = "\n")

  tf <- paste0(tempfile(), c(".tex", ".pdf"))
  writeLines(tex, tf[[1]])

  withr::with_dir(
    dirname(tf[[1]]),
    tools::texi2pdf(tf[[1]])
  )

  file.copy(from = tf[[2]], to = outfile, overwrite = overwrite)

  if (keep_source){
    file.copy(tf[[1]], paste0(outfile, ".tex"), overwrite = overwrite)
  }

}

