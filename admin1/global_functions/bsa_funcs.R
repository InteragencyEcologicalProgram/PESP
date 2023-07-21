#' Standardize Unknowns
#' 
#' purpose: standardize the different words used for "unknown" to "unknown"
#' @param df a data frame
#' 
func_unknowns <- function(df){
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