#' Standardize Unknowns
#' 
#' purpose: standardize the different words used for "unknown" to "unknown"
#' @param df a data frame
#' 
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

#' TODO: WRITE
#' standardize spp. to sp.
clean_sp <- function(df){
  df$Taxon <- stringr::str_replace_all(df$Taxon, 'spp.', 'sp.')
  df$Species <- stringr::str_replace_all(df$Taxon, 'spp.', 'sp.')
  
  return(df)
}


# Standardize Column Names ------------------------------------------------
#' TODO: WRITE
#' remove blank rows caused by measurement cols
remove_rows <- function(df){
  # subset columns with word 'measurement'
  mes_cols <- colnames(df)[grepl('Measurement', colnames(df))]
  
  # remove measurement and dimension columns
  df <- df %>%
    dplyr::select(-c(Dimension, mes_cols))
  
  # remove completely blank rows cause by measurement columns
  df <- janitor::remove_empty(df, which = 'rows')
  
  return(df)
}

#' TODO: write
#' rename columns

rename_cols <- function(df){
  df <- df %>%
    dplyr::rename(
      Date = SampleDate,
      Time = SampleTime,
      Station = StationCode,
      PhytoForm = `Colony/Filament/Individual Group Code`,
      GALD = `GALD 1`,
    ) %>%
    dplyr::mutate(
      Lab = 'BSA'
    )
  
  return(df)
}

#' TODO: write
#' calc reporting units

calc_units <- function(df){
  df <- df %>%
    dplyr::mutate(
      Units_per_mL = `Unit Abundance (# of Natural Units)` * Factor,
      Cells_per_mL = `Total Number of Cells` * Factor,
      Biovolume_per_mL = `Biovolume 1` * Factor * `Total Number of Cells`
    )
  
  return(df)
}

# TODO: write
# standardize columns from raw BSA data
# last updated: 10/2023
standardize_cols <- function(df){
  # remove blank rows
  df <- remove_rows(df)
  
  # rename columns
  df <- rename_cols(df)
  
  # calc reporting units
  df <- calc_units(df)
  
  # select and reorder relevant columns
  df <- df %>%
    select(all_of(c('Lab', 'Date', 'Time', 'Station', 'Taxon', 'Genus', 'Species', 'Units_per_mL', 'Cells_per_mL', 'Biovolume_per_mL', 'GALD', 'PhytoForm', 'Comments')))
}

#' TODO: later
# correctly identify NA values in PhytoForm
clean_phytoform <- function(df){
  df$PhytoForm[df$PhytoForm == 'n/p'] <- NA_character_
  return(df)
}

# Add QC Codes ------------------------------------------------

add_qc_col <- function(df){
  df <- df %>%
    mutate(
      QC_1 = case_when(grepl('delete|cross contamination', Comments, ignore.case = TRUE) ~ 'BadData'),
      QC_2 = case_when(grepl('did not reach|cannot meet tally|cannot meet natural unit', Comments, ignore.case = TRUE) ~ 'TallyNotMet'),
      QC_3 = case_when(grepl('degraded', Comments, ignore.case = TRUE) ~ 'Degraded'),
      QC_4 = case_when(grepl('poor preservation|poorly preserved|weak preservation|weakly preserved|fungus', Comments, ignore.case = TRUE) ~ 'PoorlyPreserved'),
      QC_5 = case_when(grepl('obscured', Comments, ignore.case = TRUE) ~ 'Obscured'),
      QC_6 = case_when(grepl('fragment\\.|diatom fragment', Comments, ignore.case = TRUE) ~ 'Fragmented'),
      QC_7 = case_when(grepl('broken diatom', Comments, ignore.case = TRUE) & !grepl('broken diatom fragment', Comments, ignore.case = TRUE) ~ 'BrokenDiatoms'),
    ) %>%
    unite(QualityCheck, starts_with('QC'), remove = TRUE, na.rm = TRUE, sep = ' ')

  df$QualityCheck[df$QualityCheck == ''] <- 'Good'

  return(df)
}

# R assigns sequentially, so column priority will always be the "highest" level of debris
add_debris_col <- function(df){
  df <- df %>%
    mutate(
      Debris =
        case_when(
          grepl('high detritus|high sediment|heavy detritus|heavy sediment', Comments, ignore.case = TRUE) ~ 'high',
          grepl('moderate detritus|moderate sediment', Comments, ignore.case = TRUE) ~ 'moderate',
          grepl('low detritus|low sediment', Comments, ignore.case = TRUE) ~ 'low',
          TRUE ~ NA_character_
        )
    )
  
  return(df)
}

# Read in Data ------------------------------------------------

read_bsa_xlsx <- function(path){
  # read in datetime columns as date
  vec_names <- names(readxl::read_excel(path, n_max = 0))
  
  # define col types
  vec_types <- ifelse(grepl('Date|Time', vec_names), 'date', 'guess')
  
  # read in data
  df <- readxl::read_excel(path, col_types = vec_types)
  
  return(df)
}


