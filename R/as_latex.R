#* @testfile manual_tests/test_output_formats

#' Convert an \R Object to Latex code
#'
#' @section Latex Packages:
#' Code created with `as_latex` assumes that the following Latex packages are
#' loaded:
#'
#' ```
#' \usepackage{booktabs}
#' \usepackage{longtable}
#' \usepackage{threeparttablex}
#'```
#'
#' @param x
#' @param ... passed on to methods
#' @param .kable_options
#'
#' @return
#' @export
#'
#' @examples
as_latex <- function(x, ..., .kable_options){
  require_knitr()
  UseMethod("as_latex")
}


#' Title
#'
#' @param x
#' @inheritParams tag_table
#' @param ...
#' @param .kable_options
#'
#' @return
#' @export
#'
#' @examples
as_latex.Tagged_table <- function(
  x,
  ...,
  .kable_options = default_kable_options
){
  meta  <- attr(x, 'meta')

  if (!is.null(meta)){
    caption <- paste(
      c(make_tag_table_print_title(meta)),
      collapse = " "
    )
    ko = c(list(caption = caption), .kable_options)
  } else {
    ko <- .kable_options
  }

  res <- NextMethod(as_latex, dd, ..., .kable_options = ko)


  res <- gsub(
    "\\\\bottomrule.\\\\endlastfoot",
    "\\\\bottomrule\n\\\\insertTableNotes\n\\\\endlastfoot",
    res
  )

  res <- sprintf(
  "\\begin{ThreePartTable}
    \\begin{TableNotes}
      \\item %s
    \\end{TableNotes}
      %s
  \\end{ThreePartTable}",
  meta$footer,
  res
  )

  res
}



#' Title
#'
#' @param x
#' @inheritParams mash_table
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
as_latex.Mashed_table <- function(
  x,
  mash_method = attr(x, 'mash_method'),
  id_vars  = attr(x, 'id_vars'),
  insert_blank_row = attr(x, 'insert_blank_row'),
  sep_height = attr(x, 'sep_height'),
  font_size = 12,
  ...
){
  sep_height = sep_height - 12 * !insert_blank_row

  res <- x %>%
    as.data.table(id_vars = id_vars) %>%
    knitr::kable(
      format = "latex",
      booktabs = TRUE,
      longtable = TRUE,
      linesep = c(rep('', length(x) - 1), sprintf("\\addlinespace[%spt]", sep_height))
    ) %>%
    kableExtra::kable_styling(latex_options = c("repeat_header"))
}



as_latex.data.frame <- function(
  x,
  ...,
  .kable_options = default_kable_options
){
  do.call(
    knitr::kable,
    args = c(list(x), .kable_options)
  ) %>%
    kableExtra::kable_styling(latex_options = c("repeat_header"))
}



default_kable_options <- list(
  format = "latex",
  booktabs = TRUE,
  longtable = TRUE
)


#' Save Table as PDF
#'
#' A convenience wrapper around [as_latex()] for directly saving an \R object
#' to \file{.pdf}.
#'
#' @param x
#' @param outfile
#' @oaram ... passed on to methods
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
  outfile = paste0(paste(substitute(x), collapse = "_"), ".pdf"),
  ...,
  overwrite = FALSE,
  papersize = "a4paper",
  orientation = "portrait",
  keep_source = FALSE,
  template = system.file("templates", "save_tex.Rmd", package = "tatoo")
){
  UseMethod("save_pdf")
}




#' @export
#' @rdname save_pdf
save_pdf.default <- function(
  x,
  outfile = paste0(make.names(paste(substitute(x), collapse = "_")), ".pdf"),
  ...,
  overwrite = FALSE,
  papersize = "a4paper",
  orientation = "portrait",
  keep_source = FALSE,
  template = system.file("templates", "save_tex.Rmd", package = "tatoo")
){
  temp <- readLines(template)

  if (file.exists(outfile)){
    if (overwrite) unlink(outfile)
    else stop(sprintf("'%s' already exists."))
  }

  tex <- as_latex(x, ...)
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

  assert_that(file.copy(from = tf[[2]], to = outfile, overwrite = overwrite))

  if (keep_source){
    file.copy(tf[[1]], paste0(outfile, ".tex"), overwrite = overwrite)
  }

  invisible(outfile)
}
