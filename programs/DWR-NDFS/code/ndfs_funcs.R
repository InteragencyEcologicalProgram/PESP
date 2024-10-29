# clean_sp <- function(df){
#   df$Taxon <- stringr::str_replace_all(df$Taxon, 'spp.', 'sp.')
#   df$Species <- stringr::str_replace_all(df$Species, 'spp.', 'sp.')
#   
#   return(df)
# }

# clean_unknowns <- function(df){
#   unknown_syns <- 'unknown|unidentified|Unidentified|Undetermined|undetermined'
#   
#   df <- df %>%
#     # update Taxon column to standardize Unknown
#     mutate(
#       Taxon = case_when(grepl(unknown_syns, Taxon, ignore.case = TRUE) ~ str_replace_all(Taxon, unknown_syns, 'Unknown'),
#                         TRUE ~ Taxon)
#     ) %>%
#     # Update Genus column if unknown Species
#     mutate(
#       Genus = case_when(grepl('Unknown', Taxon) ~ 'Unknown',
#                         is.na(Genus) ~ 'Unknown',
#                         Genus == 'Other' ~ 'Unknown',
#                         Genus == 'genus' ~ 'Unknown',
#                         TRUE ~ Genus)
#     ) %>%
#     # Update Species column in unknown
#     mutate(
#       Species = case_when(Genus == 'Unknown' ~ 'Unknown',
#                           is.na(Species) ~ 'Unknown',
#                           TRUE ~ Species)
#     )
#   return(df)
# }


# consulted Tiffany Brown (EMP) for fomulas
calc_data_ndfs <- function(df){
  df <- df %>%
    dplyr::mutate(
      Units_per_mL = round(UnitAbundance * Factor,2),
      Cells_per_mL = round(UnitAbundance * NumberOfCellsPerUnit * Factor,2),
      Biovolume_per_mL = round(Biovolume1 * UnitAbundance * NumberOfCellsPerUnit * Factor,2)
    )
  
  return(df)
}

rename_cols_ndfs <- function(df){
  df <- df %>%
    dplyr::rename(
      Station = StationCode,
      PhytoForm = Colony_Filament_IndividualGroupCode,
      DiatomSoftbody = `Diatom/SoftBody`
    ) %>%
    dplyr::mutate(
      Lab = 'BSA'
    )
  
  return(df)
}

# clean_phytoform <- function(df){
#   df$PhytoForm[df$PhytoForm == 'n/p'] <- NA_character_
#   return(df)
# }

read_bsa_xlsx <- function(path){
  # read in datetime columns as date
  vec_names <- names(readxl::read_excel(path, n_max = 0))
  
  # define col types
  vec_types <- ifelse(grepl('Date|Time', vec_names), 'date', 'guess')
  
  # read in data
  df <- readxl::read_excel(path, col_types = vec_types)
  
  return(df)
}

standardize_cols_ndfs <- function(df, meta_df){
  # rename columns
  df <- rename_cols_ndfs(df)
  
  # calc reporting units
  df <- calc_data_ndfs(df)
  
  # # fix date format
  # df$Date <- as.Date(df$Date,'%m/%d/%Y')
  
  # pull collection type
  df <- from_meta(df, meta_df, 'Sampling Method')
  
  df <- df %>% rename('SamplingMethod' = `Sampling Method`)
}

# Add QC Codes ------------------------------------------------
# add_qc_col <- function(df){
#   df <- df %>%
#     mutate(
#       QC_1 = case_when(grepl('delete|cross contamination', Comments, ignore.case = TRUE) ~ 'BadData'),
#       QC_2 = case_when(grepl('did not reach|cannot meet tally|cannot meet natural unit', Comments, ignore.case = TRUE) ~ 'TallyNotMet'),
#       QC_3 = case_when(grepl('degraded', Comments, ignore.case = TRUE) ~ 'Degraded'),
#       QC_4 = case_when(grepl('poor preservation|poorly preserved|weak preservation|weakly preserved|fungus', Comments, ignore.case = TRUE) ~ 'PoorlyPreserved'),
#       QC_5 = case_when(grepl('obscured', Comments, ignore.case = TRUE) ~ 'Obscured'),
#       QC_6 = case_when(grepl('fragment\\.|diatom fragment', Comments, ignore.case = TRUE) ~ 'Fragmented'),
#       QC_7 = case_when(grepl('broken diatom', Comments, ignore.case = TRUE) & !grepl('broken diatom fragment', Comments, ignore.case = TRUE) ~ 'BrokenDiatoms'),
#     ) %>%
#     unite(QualityCheck, starts_with('QC'), remove = TRUE, na.rm = TRUE, sep = ' ')
#   
#   df$QualityCheck[df$QualityCheck == ''] <- 'Good'
#   
#   return(df)
# }
# 
# # R assigns sequentially, so column priority will always be the "highest" level of debris
# add_debris_col <- function(df){
#   df <- df %>%
#     mutate(
#       Db_1 = case_when(
#         grepl('high detritus|high sediment|heavy detritus|heavy sediment', Comments, ignore.case = TRUE) ~ 'High',
#         grepl('moderate detritus|moderate sediment', Comments, ignore.case = TRUE) ~ 'Moderate',
#         grepl('low detritus|low sediment', Comments, ignore.case = TRUE) ~ 'Low'),
#       Db_2 = case_when(grepl('mucilaginous', Comments, ignore.case = TRUE) ~ 'Mucilaginous')
#     ) %>%
#     unite(Debris, starts_with('Db'), remove = TRUE, na.rm = TRUE, sep = ' ')
#   
#   return(df)
# }
