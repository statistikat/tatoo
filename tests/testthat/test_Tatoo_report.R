context("Tatoo_report")

tdat <- list()

for(i in seq_len(3)){
  meta <- tt_meta(
    paste0('T', i),
    'A table',
    'With a Long Title'
  )

  tdat[[i]] <- tag_table(
    data.frame(
      small = letters[i:(i+3)],
      big = LETTERS[i:(i+3)]
    ),
    meta = meta
  )
}

expect_silent(tdat <- tatoo_report(tdat))
td <- tempdir()


test_that("Tatoo_report works as expected", {
  tfile_txt <- file.path(td, 'pr.txt')
  expect_silent(save_txt(tdat, tfile_txt))
  # cat(paste(readLines(tfile_txt), collapse = '\n'))

  tfile_xlsx <- file.path(td, 'pr.xlsx')
  expect_silent(
    save_xlsx(tdat, tfile_xlsx, overwrite = TRUE)
  )
})


