clean_unknowns <- function(df){
  unknown_syns <- 'unknown|unidentified|Unidentified|Undetermined|undetermined'
  
  df <- df %>%
    # update Taxon column to standardize Unknown
    mutate(
      Taxon = case_when(grepl(unknown_syns, Taxon, ignore.case = TRUE) ~ str_replace_all(Taxon, unknown_syns, 'Unknown'),
                        TRUE ~ Taxon)
    ) %>%
    # Update Genus column if unknown Species
    mutate(
      Genus = case_when(grepl('Unknown', Taxon) ~ 'Unknown',
                        is.na(Genus) ~ 'Unknown',
                        Genus == 'Other' ~ 'Unknown',
                        Genus == 'genus' ~ 'Unknown',
                        TRUE ~ Genus)
    ) %>%
    # Update Species column in unknown
    mutate(
      Species = case_when(Genus == 'Unknown' ~ 'Unknown',
                          is.na(Species) ~ 'Unknown',
                          TRUE ~ Species)
    )
  return(df)
}

rename_cols <- function(df){
  df <- df %>%
    rename(
      'Phylum' = 'division',
      'Genus' = 'genus',
      'Taxon' = 'species',
      'Station' = 'site_abbrev',
      'Biovolume_per_mL' = 'biovolume_um3_per_ml',
      'Cells_per_mL' ='cells_per_ml',
      'CollectionType' = 'sampler'
    ) %>%
    mutate(
      'Species' = gsub('^.* ', '', Taxon),
      'Time' = hms::as_hms(mdy_hms(datetime_pst, tz = 'America/Los_Angeles')),
      'Date' = as.Date(df_data$datetime_pst, format = '%m/%d/%Y'),
      'Replicate' = case_when(replicate == 'y' ~ TRUE, replicate == 'n' ~ FALSE)
    )

  df <- df %>%
    mutate(Species =
             case_when(Genus == Species ~ 'sp.',
                       TRUE ~ Species))
}

subset_cols <- function(df){
  keep_cols <- c('Date','Time','Station','CollectionType','Replicate','Taxon','Genus','Species','Cells_per_mL','Biovolume_per_mL','QualityCheck') 
  
  df <- df %>%
    select(keep_cols)
 
  return(df) 
}

clean_sp <- function(df){
  df$Taxon <- stringr::str_replace_all(df$Taxon, 'spp.', 'sp.')
  df$Species <- stringr::str_replace_all(df$Species, 'spp.', 'sp.')
  
  return(df)
}

add_qc_col <- function(df){
  df <- df %>%
    mutate(
      QC_1 = case_when(grepl('delete|cross contamination', notes, ignore.case = TRUE) ~ 'BadData'),
      QC_2 = case_when(grepl('did not reach|cannot meet tally|cannot meet natural unit', notes, ignore.case = TRUE) ~ 'TallyNotMet'),
      QC_3 = case_when(grepl('degraded', notes, ignore.case = TRUE) ~ 'Degraded'),
      QC_4 = case_when(grepl('poor preservation|poorly preserved|weak preservation|weakly preserved|fungus', notes, ignore.case = TRUE) ~ 'PoorlyPreserved'),
      QC_5 = case_when(grepl('obscured', notes, ignore.case = TRUE) ~ 'Obscured'),
      QC_6 = case_when(grepl('fragment\\.|diatom fragment', notes, ignore.case = TRUE) ~ 'Fragmented'),
      QC_7 = case_when(grepl('broken diatom', notes, ignore.case = TRUE) & !grepl('broken diatom fragment', notes, ignore.case = TRUE) ~ 'BrokenDiatoms'),
    ) %>%
    unite(QualityCheck, starts_with('QC'), remove = TRUE, na.rm = TRUE, sep = ' ')
  
  df$QualityCheck[df$QualityCheck == ''] <- 'Good'
  
  return(df)
}

check_methods <- function(df){
  ls_methods <- unique(df$CollectionType)
  
  message(message(glue::glue('Currect collection type methods: {toString(ls_methods)}')))
}
