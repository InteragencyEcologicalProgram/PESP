
# add_qc_col --------------------------------------------------------------

test_that("add_qc_col flags key QC categories", {
  df <- tibble(
    Date = as.Date('2020-01-01'),
    Station = 'A1',
    Taxon = 'Fragilaria',
    Comments = c(
      'delete sample',
      'cross contamination',
      'CMT > 5',
      'CMT < 5',
      'cannot meet tally',
      'degraded sample',
      'poorly preserved fungus',
      'obscured view',
      'many broken diatoms',
      'mucilaginous detritus'
    ),
    Units_per_mL = c(NA, 2, 3, 4, 5, 6, 7, 8, 9, 10),
    Cells_per_mL = c(NA, 1, 3, 5, 5, 6, 7, 8, 9, 1)
  )
  
  out <- add_qc_col(df)
  
  expect_true(any(grepl('BadData', out$QualityCheck)))
  expect_true(any(grepl('CrossContamination', out$QualityCheck)))
  expect_true(any(grepl('TallyNotMet_Over5', out$QualityCheck)))
  expect_true(any(grepl('TallyNotMet_Under5', out$QualityCheck)))
  expect_true(any(grepl('TallyNotMet', out$QualityCheck)))
  expect_true(any(grepl('Degraded', out$QualityCheck)))
  expect_true(any(grepl('PoorlyPreserved', out$QualityCheck)))
  expect_true(any(grepl('Obscured', out$QualityCheck)))
  expect_true(any(grepl('BrokenDiatoms', out$QualityCheck)))
  expect_true(any(grepl('MucilaginousDetritus', out$QualityCheck)))
})

test_that("add_qc_col collapses Over5 and Under5 into TallyNotMet", {
  df <- tibble(
    Date = as.Date('2020-01-01'),
    Station = 'A1',
    Taxon = 'Fragilaria',
    Comments = 'CMT > 5 and CMT < 5',
    Units_per_mL = 1,
    Cells_per_mL = 1
  )
  
  out <- add_qc_col(df)
  expect_false(any(grepl('Over5|Under5', out$QualityCheck)))
  expect_true(any(grepl('TallyNotMet', out$QualityCheck)))
})

test_that("add_qc_col assigns Unknown when no comment_col", {
  df <- tibble(Date = as.Date('2020-01-01'), Station = 'A1', Taxon = 'Fragilaria')
  out <- add_qc_col(df, comment_col = NULL)
  expect_equal(unique(out$QualityCheck), 'Unknown')
})

test_that("add_qc_col flags UnitsExceedCells", {
  df <- tibble(
    Date = as.Date('2020-01-01'),
    Station = 'A1',
    Taxon = 'Fragilaria',
    Comments = 'ok',
    Units_per_mL = 10,
    Cells_per_mL = 5
  )
  out <- add_qc_col(df)
  expect_true(any(grepl('UnitsExceedCells', out$QualityCheck)))
})


# add_debris_col ----------------------------------------------------------

test_that("add_debris_col assigns correct debris levels", {
  df <- tibble(
    Comments = c(
      'high detritus and heavy sediment',
      'moderate detritus',
      'low sediment',
      'light detritus',
      'no mention of debris'
    )
  )
  out <- add_debris_col(df)
  
  expect_equal(out$Debris[1], 'High')
  expect_equal(out$Debris[2], 'Moderate')
  expect_equal(out$Debris[3], 'Low')
  expect_equal(out$Debris[4], 'Low')
  expect_equal(out$Debris[5], 'None')
})

test_that("add_debris_col returns Unknown when no comment_col provided", {
  df <- tibble(a = 1:3)
  out <- add_debris_col(df, comment_col = NULL)
  expect_true(all(out$Debris == 'Unknown'))
})

test_that("add_debris_col handles mixed levels with High priority", {
  df <- tibble(Comments = 'high detritus and low sediment')
  out <- add_debris_col(df)
  expect_equal(out$Debris, 'High')
})


# add_notes_col -----------------------------------------------------------

test_that("add_notes_col detects note keywords in Taxon and Comments", {
  df <- tibble(
    Taxon = c('Fragilaria cyst', 'Gomphonema (fragmented diatoms)', 'Unknown'),
    Comments = c('cyst found', 'flagellate observed', 'no notes')
  )
  
  out <- add_notes_col(df)
  
  # check main detections
  expect_true(any(grepl('Cyst', out$Notes)))
  expect_true(any(grepl('Flagellate', out$Notes)))
  expect_true(any(grepl('FragmentedDiatoms', out$Notes)))
  
  # ensure "Unknown" normalized to "Unknown sp."
  expect_true(any(out$Taxon == 'Unknown sp.'))
})

test_that("add_notes_col removes note terms from Taxon", {
  df <- tibble(Taxon = 'Navicula filament', Comments = '')
  out <- add_notes_col(df)
  expect_false(any(grepl('filament', out$Taxon, ignore.case = TRUE)))
  expect_equal(out$Notes, 'Filament')
})

test_that("add_notes_col identifies unmatched parentheses content", {
  df <- tibble(Taxon = 'Cymbella (weird note)', Comments = '')
  out <- add_notes_col(df)
  log_data <- attr(out, 'log')$unmatched_notes
  expect_true(any(grepl('weird note', log_data$ParenContent)))
})

test_that("add_notes_col returns 'NoNote' when nothing matches", {
  df <- tibble(Taxon = 'Fragilaria', Comments = 'none')
  out <- add_notes_col(df)
  expect_equal(out$Notes, 'NoNote')
})

test_that("add_notes_col logs taxon changes when note terms removed", {
  df <- tibble(Taxon = 'Aulacoseira cyst', Comments = '')
  out <- add_notes_col(df)
  log_data <- attr(out, 'log')$taxa_notes
  expect_true(any(grepl('Aulacoseira cyst', log_data$OldTaxon)))
  expect_true(any(grepl('Aulacoseira', log_data$UpdatedTaxon)))
})



# add_meta_col ------------------------------------------------------------

test_that("add_meta_col correctly joins metadata by date range", {
  mock_meta <- tibble(
    Survey = 'EMP',
    `Starting Date` = as.Date(c('2020-01-01', '2020-06-01')),
    `Ending Date`   = as.Date(c('2020-05-31', '2020-12-31')),
    Analyst = c('Alice', 'Bob')
  )
  
  df <- tibble(Date = seq(as.Date('2020-01-15'), as.Date('2020-12-15'), by = 'month'))
  
  out <- add_meta_col(
    df,
    program = 'EMP',
    col_name = Analyst,
    read_func = function(x) mock_meta
  )
  
  # first half (Jan–May) should map to Alice
  expect_true(all(out$Analyst[1:5] == 'Alice'))
  
  # second half (Jun–Dec) should map to Bob
  expect_true(all(out$Analyst[6:12] == 'Bob'))
  
  # confirm no missing values
  expect_true(all(!is.na(out$Analyst)))
})

test_that("add_meta_col stops if column missing in metadata", {
  mock_meta <- tibble(
    Survey = 'EMP',
    `Starting Date` = Sys.Date(),
    `Ending Date` = Sys.Date()
  )
  
  df <- tibble(Date = Sys.Date())
  
  expect_error(
    add_meta_col(df, 'EMP', col_name = Analyst,
                 read_func = function(x) mock_meta)
  )
})

test_that("add_meta_col stops if match_cols missing", {
  mock_meta <- tibble(
    Survey = 'EMP',
    `Starting Date` = Sys.Date(),
    `Ending Date` = Sys.Date(),
    Station = 'A',
    Analyst = 'Bob'
  )
  
  df <- tibble(Date = Sys.Date())
  
  expect_error(
    add_meta_col(
      df,
      'EMP',
      col_name = Analyst,
      match_cols = 'Station',
      read_func = function(x) mock_meta
    )
  )
})

test_that("add_meta_col respects match_cols filtering", {
  mock_meta <- tibble(
    Survey = 'EMP',
    `Starting Date` = as.Date(c('2020-01-01', '2020-01-01')),
    `Ending Date`   = as.Date(c('2020-12-31', '2020-12-31')),
    Station = c('A', 'B'),
    Analyst = c('Alice', 'Bob')
  )
  
  df <- tibble(
    Date = rep(as.Date('2020-06-01'), 2),
    Station = c('A', 'B')
  )
  
  out <- add_meta_col(
    df,
    'EMP',
    col_name = Analyst,
    match_cols = 'Station',
    read_func = function(x) mock_meta
  )
  
  expect_equal(out$Analyst, c('Alice', 'Bob'))
})

