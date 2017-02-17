context("Comp_table")



test_that("Comp_table works as expected", {
  # Generate test data
    tdat <- list()
    for(i in seq_len(3)){
      tdat[[i]] <- data.frame(
        id = 1:6,
        small = letters[i:(i+5)],
        tall = LETTERS[i:(i+5)]
      )
    }



  # Ursula wants to combine several tables to a Comp_table (side-by-side tables)

  ## Titles must be specified (otherwise comp_table would just be a wrapper for
  ## cbind)
    expect_error(tres <- comp_table(tdat[[1]], tdat[[2]], tdat[[3]]))
    expect_error(tres <- comp_table_list(tdat))

  ## If "tables" is a named list, titles are automatically set to element names
    names(tdat) <- c('a', 'b', 'c')
    expect_silent(tres <- comp_table_list(tdat))
    expect_s3_class(tres, 'Comp_table')

  ## Alternatively, table names can be specified manually
    names(tdat) <- NULL
    expect_silent(tres <- comp_table_list(
      tdat,
      titles = c('tab1', 'tab2', 'tab3')
    ))
    expect_s3_class(tres, 'Comp_table')


  # Ursulas table have an ID column. She wants to merge them, instead of
  # cbinding them. This has the advantage of avoiding duplicate ID columns
  # and ensuring data integrity
    expect_silent(tres <- comp_table(
      tdat[[1]], tdat[[2]], tdat[[3]],
      titles = c('tab1', 'tab2', 'tab3'),
      by = 'id'
    ))
})



test_that("xlsx output for comp_tables works", {
  # Generate test data
    tdat <- list()
    for(i in seq_len(3)){
      tdat[[i]] <- data.frame(
        id = 1:6,
        small = letters[i:(i+5)],
        tall = LETTERS[i:(i+5)]
      )
    }

    expect_silent(tres <- comp_table_list(
      tdat,
      titles = c('tab1', 'tab2', 'tab3')
    ))

    pub_tres <- pub_table(
      tres,
      pub_table_meta(
        't1',
        'comp pub tab',
        'a composite pub table',
        'for a testing purpose'
      ))



  # Ursula wants to export her comp_table as .xlsx
  # (the xlsx files have to be checked manually)
    td <- tempdir()
    wb <- as_workbook(tres)
    outfile <- file.path(td, 'comp_table.xlsx')

    expect_silent(openxlsx::saveWorkbook(wb, outfile, overwrite = TRUE))
    pub_wb    <- as_workbook(pub_tres)
    outfile <- file.path(td, 'pub_comp_table.xlsx')
    expect_silent(openxlsx::saveWorkbook(pub_wb, outfile, overwrite = TRUE))
    # hammr::excel(outfile)


  # When exporting comp_tables created with "by", the super-headings should
  # not cover the id_vars
    expect_silent(tres <- comp_table_list(
      tdat,
      titles = c('tab1', 'tab2', 'tab3'),
      by = 'id'
    ))
    wb <- as_workbook(tres)

    outfile <- file.path(td, 'pub_comp_table_idvars.xlsx')
    expect_silent(openxlsx::saveWorkbook(wb, outfile, overwrite = TRUE))
    # hammr::excel(outfile)


    outfile <- file.path(td, 'pub_comp_table_meta.xlsx')
    save_xlsx(pub_tres, outfile)
    #hammr::excel(outfile)

})
