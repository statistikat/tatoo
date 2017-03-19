context("Stacked_table")

#* @testing stack_table
#* @testing stacked_table

test_that("Stacked_table works as expected", {

  # Generate test data
    tm <- tt_meta(
      table_id = 't001',
      title = 'Table 1',
      longtitle = 'Table of Numbers',
      subtitle = 'A table that contains numbers but maybe also letters',
      footer = 'a footer'
    )
    tdat <- data.frame(
      x = letters[1:5],
      y = letters[10:14]
    )
    expect_silent(tmeta <- tag_table(tdat, meta = tm))


    tdat1 <- data.frame(
      numbers = c(1.434, 190.3, 228.311, 5.210, 4321543),
      animals = c('dog', 'cat', 'camel', 'pig', 'mouse'),
      stringsAsFactors = FALSE
    )
    tdat2 <- data.frame(
      numbers = c(1, 290, 0.311, 0.210, 1000),
      animals = c('god', 'tac', 'lemac', 'gip', 'esuom'),
      stringsAsFactors = FALSE
    )
    expect_silent(tmash <- mash_table(
      tdat1, tdat2, meta = tt_meta('t02', 'a mash table'))
    )


    tdat <- list()
    for(i in seq_len(3)){
      tdat[[i]] <- data.frame(
        id = 1:6,
        small = letters[i:(i+5)],
        tall = LETTERS[i:(i+5)]
      )
    }

    names(tdat) <- c('tab1', 'tab2', 'tab3')

    tcomp <- comp_table_list(
      tdat,
      meta = tt_meta('t03', 'a comp table')
    )


  # Constructor works
    tdat <- stack_table(tmeta, tmash, tcomp, meta = tt_meta(
      table_id = 'rp1',
      title = 'stack table 1',
      longtitle = c(
        'stack table 1 is a stack of tables',
        'with a very long title',
        'that spans several rows'),
      subtitle = 'with a subtitle',
      footer = c('that has a footer also', 'which goes over man lines')
    ))
})
