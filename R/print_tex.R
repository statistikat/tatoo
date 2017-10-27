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


save_pdf <- function(
  x,
  outfile,
  mash_method = attr(x, 'mash_method'),
  id_vars  = attr(x, 'id_vars'),
  insert_blank_row = attr(x, 'insert_blank_row'),
  overwrite = FALSE,
  template = system.file("templates", "save_tex.Rmd", package = "tatoo"),
  keep_source = FALSE
){
  temp <- readLines(template)
  tex <- print_tex(x, insert_blank_row = insert_blank_row)
  rmd <- paste(gsub("{TABLE}", tex, temp, fixed = TRUE), collapse = "\n")
  tf <- tempfile()
  writeLines(rmd, tf)

  if (keep_source){
    of_source <- paste0(of, ".Rmd")
    file.copy(tf, of)
  }

  rmarkdown::render(
    tf,
    output_format = "pdf_document",
    output_file = outfile
  )
}

