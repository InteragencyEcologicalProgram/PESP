
# clean_unknowns ----------------------------------------------------------

test_that('clean_unknowns standardizes unknown taxa correctly', {
  
  # example data
  df <- tibble(
    Taxon = c(
      'Unknown species',
      'unidentified diatom',
      'Undetermined green algae',
      'Genus spp.',
      'Genus sp X',
      'cf. Unknown diatom',
      'Unknown sp.',
      'Unknown sp. X',
      'Unknown (note)',
      'Microcystis cf. aeruginosa',
      'Genus sp. 1'
    )
  )
  
  # run function with both std_sp and std_suffix = TRUE
  cleaned <- clean_unknowns(df, std_sp = TRUE, std_suffix = TRUE)
  
  expect_s3_class(cleaned, 'tbl_df')
  expect_true('Taxon' %in% names(cleaned))

  # specific cases
  expect_equal(cleaned$Taxon[1], 'Unknown species')
  expect_equal(cleaned$Taxon[2], 'Unknown diatom')
  expect_equal(cleaned$Taxon[3], 'Unknown green algae')
  expect_equal(cleaned$Taxon[4], 'Genus sp.')
  expect_equal(cleaned$Taxon[5], 'Genus sp.')
  expect_equal(cleaned$Taxon[6], 'Unknown diatom')
  expect_equal(cleaned$Taxon[7], 'Unknown sp.')
  expect_equal(cleaned$Taxon[8], 'Unknown sp.')
  expect_equal(cleaned$Taxon[9], 'Unknown sp.')
  expect_equal(cleaned$Taxon[10], 'Microcystis cf. aeruginosa')
  expect_equal(cleaned$Taxon[11], 'Genus sp.')
  
  # check that log attribute exists
  expect_true(!is.null(attr(cleaned, 'log')))
  expect_true('clean_unknowns' %in% names(attr(cleaned, 'log')))
  
  log_tbl <- attr(cleaned, 'log')$clean_unknowns
  expect_s3_class(log_tbl, 'tbl_df')
  expect_true(all(c('OrigTaxon', 'UpdatedTaxon') %in% names(log_tbl)))
  
  # confirm log only contains changed rows
  expect_true(all(log_tbl$OrigTaxon != log_tbl$UpdatedTaxon))
})

test_that('clean_unknowns handles std_sp and std_suffix flags', {
  df <- tibble(Taxon = c('Genus spp', 'Genus sp X', 'Unknown X sp.'))
  
  cleaned_sp <- clean_unknowns(df, std_sp = TRUE, std_suffix = FALSE)
  cleaned_suffix <- clean_unknowns(df, std_sp = FALSE, std_suffix = TRUE)
  
  # when std_sp = TRUE, spp -> sp.
  expect_equal(cleaned_sp$Taxon[1], 'Genus sp.')
  
  # when std_suffix = TRUE, sp. X -> sp.
  expect_equal(cleaned_suffix$Taxon[2], 'Genus sp.')
  
  # when unknown sp., trailing sp. removed
  expect_equal(cleaned_suffix$Taxon[3], 'Unknown x')
})


# correct_taxon_typos -----------------------------------------------------

test_that('correct_taxon_typos standardizes and corrects taxa names properly', {
  # test data
  df_typos <- tibble(
    Taxon = c('Microcystis aerugino', 'Chlamydomonas reinharti'),
    TaxonCorrected = c('Microcystis aeruginosa', 'Chlamydomonas reinhardtii')
  )
  
  df <- tibble(
    Taxon = c(
      'Microcystis aerugino',          # typo
      'Chlamydomonas cf reinharti',    # missing period (cf.) and typo
      'Navicula cf. pennata',      # correct
      'Chroococcus Dispersus',         # capitalized
      'chroococcus dispersus',         # all lowercase
      'Cyclotella sp..',               # too many periods
      'Cyclotella sp.'                 # correct
    )
  )
  
  # use custom inline read_func returning our fake typo table
  fake_reader <- function(path) df_typos
  
  cleaned <- correct_taxon_typos(df, read_func = fake_reader)
  
  # structure
  expect_s3_class(cleaned, 'tbl_df')
  expect_true('Taxon' %in% names(cleaned))
  expect_true(!is.null(attr(cleaned, 'log')))
  expect_true('taxon_corrections' %in% names(attr(cleaned, 'log')))
  
  log_tbl <- attr(cleaned, 'log')$taxon_corrections
  expect_s3_class(log_tbl, 'tbl_df')
  expect_true(all(c('OrigTaxon', 'UpdatedTaxon') %in% names(log_tbl)))
  
  # expected transformations
  expect_equal(cleaned$Taxon[1], 'Microcystis aeruginosa')
  expect_equal(cleaned$Taxon[2], 'Chlamydomonas cf. reinhardtii')
  expect_equal(cleaned$Taxon[3], 'Navicula cf. pennata')
  expect_equal(cleaned$Taxon[4], 'Chroococcus dispersus')
  expect_equal(cleaned$Taxon[5], 'Chroococcus dispersus')
  expect_equal(cleaned$Taxon[6], 'Cyclotella sp.')
  expect_equal(cleaned$Taxon[7], 'Cyclotella sp.')
  
  # log validity
  expect_true(all(log_tbl$OrigTaxon %in% df$Taxon))
  expect_true(all(log_tbl$OrigTaxon != log_tbl$UpdatedTaxon))
})


# update_synonyms ---------------------------------------------------------

test_that('update_synonyms resolves and logs synonym chains correctly', {
  
  # fake synonym table
  df_phyto <- tibble(
    Taxon = c(
      'Melosira italica',
      'Aulacoseira italica',
      'Cyclotella meneghiniana',
      'Genus oldspecies',
      'Genus newspecies',
      'Navicula pennata',
      'Genus a',
      'Genus b'
    ),
    CurrentTaxon = c(
      'Aulacoseira italica',              # synonym chain 1
      'None',                             # terminal
      'Stephanocyclus meneghinianus',     # synonym chain 2
      'Genus newspecies',                 # chain to newspecies
      'None',
      'None',
      'Genus b',                          # multi-synonym chain
      'Genus c'                           # multi-synonym chain
    )
  )
  
  # fake reader
  fake_reader <- function() df_phyto
  
  # input dataframe
  df <- tibble(
    Taxon = c(
      'Melosira italica',                  # chain Melosira -> Aulacoseira italica
      'Cyclotella meneghiniana',           # chain Cyclotella -> Stephanocyclus meneghinianus
      'Cyclotella cf. meneghiniana',       # cf. form resolves to Cyclotella cf. meneghiniana
      'Genus oldspecies',                  # chain to Genus newspecies
      'cf. Genus oldspecies',              # front cf.
      'Navicula salinarum var. rostrata',  # variety synonym
      'Navicula pennata',                  # no change (terminal)
      'Genus a'                            # chain Genus a -> Genus b -> Genus c
    )
  )
  
  # call function
  cleaned <- update_synonyms(df, read_func = fake_reader)
  
  # --- structure checks ---
  expect_s3_class(cleaned, 'tbl_df')
  expect_true(all(c('OrigTaxon', 'Taxon') %in% names(cleaned)))
  expect_true(!is.null(attr(cleaned, 'log')))
  expect_true('synonym_updates' %in% names(attr(cleaned, 'log')))
  
  log_tbl <- attr(cleaned, 'log')$synonym_updates
  expect_s3_class(log_tbl, 'tbl_df')
  expect_true(all(c('OrigTaxon', 'UpdatedTaxon') %in% names(log_tbl)))
  
  # --- expected synonym resolutions ---
  expect_equal(cleaned$Taxon[1], 'Aulacoseira italica')                 # simple chain
  expect_equal(cleaned$Taxon[2], 'Stephanocyclus meneghinianus')        # direct synonym
  expect_equal(cleaned$Taxon[3], 'Stephanocyclus cf. meneghinianus')    # cf. middle
  expect_equal(cleaned$Taxon[4], 'Genus newspecies')                    # chain resolved
  expect_equal(cleaned$Taxon[5], 'cf. Genus newspecies')                # cf. front
  expect_equal(cleaned$Taxon[7], 'Navicula pennata')                    # unchanged
  expect_equal(cleaned$Taxon[8], 'Genus c')                             # multi-step chain
  
  # unchanged taxon should have NA in OrigTaxon
  expect_true(is.na(cleaned$OrigTaxon[7]))
  
  # all changed taxa should appear in the log
  expect_true(all(log_tbl$OrigTaxon %in% df$Taxon))
  expect_true(all(log_tbl$OrigTaxon != log_tbl$UpdatedTaxon))
})


# higher_lvl_taxa ---------------------------------------------------------

test_that('higher_lvl_taxa appends taxonomy fields and handles cf. logic', {
  # fake taxonomy table
  fake_read_func <- function() {
    tibble(
      Taxon = c(
        'Chroococcus dispersus',
        'Chroococcus sp.',
        'Gonyaulax verior',
        'Sourniaea diacantha',
        'Sourniaea sp.'
      ),
      Kingdom = c('Bacteria','Bacteria','Chromista','Chromista','Chromista'),
      Phylum = c('Cyanobacteria','Cyanobacteria','Dinoflagellata','Dinoflagellata','Dinoflagellata'),
      Class = c('Cyanophyceae','Cyanophyceae','Dinophyceae','Dinophyceae','Dinophyceae'),
      AlgalGroup = c('Cyanobacteria','Cyanobacteria','Dinoflagellates','Dinoflagellates','Dinoflagellates'),
      Genus = c('Chroococcus','Chroococcus','Gonyaulax','Sourniaea','Sourniaea'),
      Species = c('dispersus','sp.','verior','diacantha','sp.'),
      CurrentTaxon = c(NA,NA,'Sourniaea diacantha',NA,NA)
    )
  }
  
  # sample df
  df <- tibble(
    OrigTaxon = c(NA,NA,'Gonyaulax verior',NA,'Gonyaulax verior',NA,NA),
    Taxon = c(
      'Chroococcus dispersus',            # up-to-date species
      'Chroococcus sp.',                  # up-to-date genus
      'Sourniaea diacantha',              # capitalization issue
      'Chroococcus dispersus cf.',        # cf. at end
      'Sourniaea cf. diacantha',          # cf. in middle
      'cf. Chroococcus dispersus',        # cf. at start
      'Sourniaea sp. 1'                   # suffix
    )
  )
  
  # --- program mode ---
  df_prog <- higher_lvl_taxa(df, after_col = 'Taxon', std_type = 'program', read_func = fake_read_func)
  
  df_prog <<- df_prog
  
  # should contain classification columns
  expect_true(all(c('Kingdom','Phylum','Class','AlgalGroup','Genus','Species') %in% names(df_prog)))
  
  # unmatched taxa log exists and has correct structure
  unmatched <- attr(df_prog, 'log')$unmatched_taxa
  expect_s3_class(unmatched, 'data.frame')
  expect_true(all(c('PureTaxon','Taxon') %in% names(unmatched)))
  
  # capitalization normalized
  expect_true(any(grepl('Sourniaea diacantha', df_prog$Taxon)))
  
  # --- PESP mode ---
  df_pesp <- higher_lvl_taxa(df, after_col = 'Taxon', std_type = 'pesp', read_func = fake_read_func)
  
  # capitalization normalized
  expect_true(any(grepl('Sourniaea diacantha', df_prog$Taxon)))
  
  # 'Genus cf.' standardized to 'Genus sp.'
  expect_true(any(grepl('Sourniaea sp\\.', df_pesp$Taxon)))
  print(df_pesp$Taxon)
  
  # 'cf. Genus' replaced with 'Unknown <algal group>'
  expect_true(any(grepl('Unknown cyanobacterium', df_pesp$Taxon)))

  # unmatched taxa log still present and structured
  unmatched2 <- attr(df_pesp, 'log')$unmatched_taxa
  expect_s3_class(unmatched2, 'data.frame')
})



# combine_taxa ------------------------------------------------------------

test_that('combine_taxa calculates densities correctly, Notes/QualityCheck/Debris/PhytoForm/GALD handled correctly, OrigTaxon rules exist', {
  
  # create example dataframe
  df <- tibble(
    Date = as.Date(rep('2024-05-01', 8)),
    Station = c('A','A','A','A','B','B','C','C'),
    Kingdom = c('Bacteria','Bacteria','Bacteria','Bacteria','Plantae','Plantae','Bacteria','Bacteria'),
    Phylum  = c('Cyanobacteria','Cyanobacteria','Cyanobacteria','Cyanobacteria',
                'Bacillariophyta','Bacillariophyta','Cyanobacteria','Cyanobacteria'),
    Class   = c('Cyanophyceae','Cyanophyceae','Cyanophyceae','Cyanophyceae',
                'Coscinodiscophyceae','Fragilariophyceae','Cyanophyceae','Cyanophyceae'),
    
    OrigTaxon = c(
      'Anacystis cyanea', 'Another example', NA,
      NA,
      NA, NA,
      NA, NA
    ),
    
    Taxon = c(
      'Microcystis aeruginosa','Microcystis aeruginosa','Microcystis aeruginosa',
      'Anabaena flos-aquae',
      'Aulacoseira ambigua','Aulacoseira ambigua',
      'Microcystis flos-aquae','Microcystis flos-aquae'
    ),
    
    Biovolume_per_mL = c(1,2,3,5,10,20,1,2),
    Units_per_mL     = c(10,20,30,40,50,60,5,5),
    Cells_per_mL     = c(100,200,300,400,500,600,10,20),
    
    GALD = c(5, 7, 4, 2, 10, 12, NA, 15),
    
    Notes = c('good','good','NoNote','bad','NoNote','Unknown','a','b'),
    QualityCheck = c('NoCode','A1','NoCode','B2','A2','B3','X1','X2'),
    Debris = c('Low','Moderate','None','High','Low','Moderate','Low','Low'),
    PhytoForm = c('c','c','f','f','i','i','c','c')
  )
  
  out <- combine_taxa(df)
  
  # Microcystis aeruginosa @ A (merged)
  mA <- out %>% filter(Station == 'A', Taxon == 'Microcystis aeruginosa')
  expect_equal(mA$OrigTaxon,
               'Anacystis cyanea; Another example; Microcystis aeruginosa')
  expect_equal(mA$Biovolume_per_mL, 6)
  expect_equal(mA$Units_per_mL, 60)
  expect_equal(mA$Cells_per_mL, 600)
  expect_equal(mA$Notes, 'good MultipleEntries')
  expect_equal(mA$QualityCheck, 'A1')
  expect_equal(mA$Debris, 'Moderate, Low, None')
  expect_equal(mA$GALD, 7)
  
  # Anabaena flos-aquae @ A (not merged)
  aA <- out %>% filter(Station == 'A', Taxon == 'Anabaena flos-aquae')
  expect_equal(aA$Biovolume_per_mL, 5)
  expect_equal(aA$Notes, 'bad')
  expect_true(is.na(aA$OrigTaxon))
  expect_equal(aA$GALD, 2)
  
  # Station B (K/P/C mismatch should produce two Unknown taxa)
  uB <- out %>% filter(Station == 'B', stringr::str_detect(Taxon, '^Unknown'))
  expect_equal(nrow(uB), 2)
  expect_true(all(c('Unknown coscinodiscophyceae','Unknown fragilariophyceae') %in% uB$Taxon))
  expect_true(all(uB$Biovolume_per_mL %in% c(10,20)))
  expect_true(all(uB$Notes %in% c('NoNote','Unknown')))
  expect_equal(sort(uB$GALD), c(10, 12))  # each keeps its own max GALD
  
  # Microcystis flos-aquae @ C (merged)
  mC <- out %>% filter(Station == 'C', Taxon == 'Microcystis flos-aquae')
  expect_equal(mC$Biovolume_per_mL, 3)
  expect_equal(mC$Units_per_mL, 10)
  expect_equal(mC$Cells_per_mL, 30)
  expect_equal(mC$Notes, 'a b MultipleEntries')
  expect_equal(mC$QualityCheck, 'X1 X2')
  expect_equal(mC$Debris, 'Low')
  expect_true(is.na(mC$OrigTaxon))
  expect_equal(mC$GALD, 15) 
  
  # attributes log present
  lg <- attr(out, 'log')
  expect_true(is.list(lg))
  expect_true(all(c('combined_taxa','combined_conflicts') %in% names(lg)))
})
