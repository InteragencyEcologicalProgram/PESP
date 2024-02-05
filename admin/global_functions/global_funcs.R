#' create object "not in" for logic purposes
'%!in%' <- function(x,y)!('%in%'(x,y))

`%>%` <- magrittr::`%>%`

#' TODO: later
#' 

read_quiet_csv <- function(fp){
  df <- suppressWarnings(readr::read_csv(fp, show_col_types = FALSE))
  
  return(df)
}

# abs path

abs_data_path <- function(fp_rel = NULL) {
  fp_full <- 'California Department of Water Resources/Phytoplankton synthesis - Documents/'
  
  if (is.null(fp_rel)) {
    fp_abs <- normalizePath(file.path(Sys.getenv('USERPROFILE'), fp_full))
  } else {
    fp_abs <- normalizePath(file.path(Sys.getenv('USERPROFILE'), fp_full, fp_rel))
  }
  
  return(fp_abs)
}

#' Creates a "quality check" column based on a comment column
#'
#'@param df A data frame
#'@param comment_col the name (as a string) of the comment column
#'@return A data frame with a comment column added
#' 
add_qc_col <- function(df, comment_col){
  df <- df %>%
    mutate(
      QC_1 = case_when(grepl('delete|cross contamination', comment_col, ignore.case = TRUE) ~ 'BadData'),
      QC_2 = case_when(grepl('did not reach|cannot meet tally|cannot meet natural unit', comment_col, ignore.case = TRUE) ~ 'TallyNotMet'),
      QC_3 = case_when(grepl('degraded', comment_col, ignore.case = TRUE) ~ 'Degraded'),
      QC_4 = case_when(grepl('poor preservation|poorly preserved|weak preservation|weakly preserved|fungus', comment_col, ignore.case = TRUE) ~ 'PoorlyPreserved'),
      QC_5 = case_when(grepl('obscured', comment_col, ignore.case = TRUE) ~ 'Obscured'),
      QC_6 = case_when(grepl('fragment\\.|diatom fragment', comment_col, ignore.case = TRUE) ~ 'Fragmented'),
      QC_7 = case_when(grepl('broken diatom', comment_col, ignore.case = TRUE) & !grepl('broken diatom fragment', comment_col, ignore.case = TRUE) ~ 'BrokenDiatoms'),
    ) %>%
    unite(QualityCheck, starts_with('QC'), remove = TRUE, na.rm = TRUE, sep = ' ')
  
  df$QualityCheck[df$QualityCheck == ''] <- 'Good'
  
  return(df)
}

#' Creates a "debris" column based on a comment column
#'
#'@param df A data frame
#'@param comment_col the name (as a string) of the comment column
#'@return A data frame with a comment column added
#'@details
#'R assigns sequentially, so column priority will always be the "highest" level of debris (for Db_1)
#'
add_debris_col <- function(df, comment_col){
  df <- df %>%
    mutate(
      Db_1 = case_when(
        grepl('high detritus|high sediment|heavy detritus|heavy sediment', comment_col, ignore.case = TRUE) ~ 'High',
        grepl('moderate detritus|moderate sediment', comment_col, ignore.case = TRUE) ~ 'Moderate',
        grepl('low detritus|low sediment', comment_col, ignore.case = TRUE) ~ 'Low'),
      Db_2 = case_when(grepl('mucilaginous', comment_col, ignore.case = TRUE) ~ 'Mucilaginous')
    ) %>%
    unite(Debris, starts_with('Db'), remove = TRUE, na.rm = TRUE, sep = ' ')
  
  return(df)
}

# clean EMP??? funcs ---------------------------------------------------------

#' TODO: later (maybe delete)
parse_dates <- function(df, col){
  df[[col]] <- lubridate::parse_date_time(df[[col]], c('%m/%d/%Y'))
  return(df)
}


# read in EDI -------------------------------------------------------------

# TODO: MOVE
get_edi_file = function(pkg_id, fnames, verbose = TRUE){
  # get revision
  revision_url = glue::glue("https://pasta.lternet.edu/package/eml/edi/{pkg_id}")
  all_revisions = readLines(revision_url, warn = FALSE) 
  latest_revision = tail(all_revisions, 1)
  if (verbose) {
    message("Latest revision: ", latest_revision)
  }
  # get entities 
  pkg_url = glue::glue("https://pasta.lternet.edu/package/data/eml/edi/{pkg_id}/{latest_revision}")
  all_entities = readLines(pkg_url, warn = FALSE)
  name_urls = glue::glue("https://pasta.lternet.edu/package/name/eml/edi/{pkg_id}/{latest_revision}/{all_entities}")
  names(all_entities) = purrr::map_chr(name_urls, readLines, warn = FALSE)
  if (verbose) {
    message("Package contains files:\n", 
            stringr::str_c("    ", names(all_entities), sep = "", collapse = "\n"))
  }
  # select entities that match fnames
  fname_regex = stringr::str_c(glue::glue("({fnames})"), collapse = "|")
  included_entities = all_entities[stringr::str_detect(names(all_entities), fname_regex)]
  if(length(included_entities) != length(fnames)){
    stop("Not all specified filenames are included in package")
  }
  # download data
  if (verbose) {
    message("Downloading files:\n",
            stringr::str_c("    ", names(included_entities), sep = "", collapse = "\n"))
  }
  dfs = purrr::map(glue::glue("https://portal.edirepository.org/nis/dataviewer?packageid=edi.{pkg_id}.{latest_revision}&entityid={included_entities}"),
                   readr::read_csv, guess_max = 1000000)
  names(dfs) = names(included_entities)
  dfs
}



# add in higher level taxa ------------------------------------------------

higher_lvl_taxa <- function(df){
  # subset synonym/taxon columns from synonym df
  df_syn <- df_syn %>%
    select(c('Kingdom':'AlgalGroup','Taxon','CurrentTaxon')) %>%
    rename(PureTaxon = Taxon)
  
  # standardize spp. to sp. in data df
  df <- clean_sp(df)
  
  # standardize unknown names in data df before joining
  # # TODO: base this on a csv (for easier editing)
  df <- df %>%
    mutate(
      Taxon = case_when(
        Taxon == 'LGBs' ~ 'Little Green Balls',
        Taxon == 'Unknown Banana Blue Green' | Taxon == 'Unknown Cyanobacteria sp.' ~ 'Unknown cyanobacterium',
        Taxon == 'Unknown centrales sp.' | Taxon == 'Unknown centric sp.' | Taxon == 'Unknown Centric diatom'  ~ 'Unknown centric diatom',
        Taxon == 'Unknown Chlorophyte alga sp.' | Taxon == 'Unknown Chlorophyte filament' ~ 'Unknown green alga',
        Taxon == 'Unknown dinoflagellate sp.' | Taxon == 'Unknown Dinoflagellate sp.' | Taxon == 'small dinoflagellates'~ 'Unknown dinoflagellate',
        Taxon == 'Unknown girdle sp.' | Taxon == 'Unknown pennales sp.' | Taxon == 'Unknown pennate girdle sp.' ~ 'Unknown pennate diatom',
        Taxon == 'Small nano-flagellates' | Taxon == 'small nano-flagellates' ~ 'Unknown nanoflagellate',
        Taxon == 'Unknown Euglenoids' ~ 'Unknown euglenoid',
        Taxon == 'Unknown Genus' ~ 'Unknown genus',
        Taxon == 'Unknown Algae' ~ 'Unknown',
        TRUE ~ Taxon),
      PureTaxon = gsub('cf\\. ', '', Taxon)
    )
  
  # add higher level taxa to data df (joined df)
  df_joined <- df %>%
    left_join(df_syn, by = 'PureTaxon') %>%
    select(-c(ends_with('.y'), ends_with('.x'), 'PureTaxon')) %>%
    relocate(c(Taxon, Kingdom, Phylum, Class, AlgalGroup), .after = Station) %>%
    relocate(c(Genus, Species), .after = AlgalGroup)
  
  return(df_joined)
}

# Update Syonyms
update_synonyms <- function(df){
  # update taxon names with current synonymous taxon
  df <- df %>%
    mutate(
      Genus = case_when(Taxon == CurrentTaxon | grepl('None|Unknown',CurrentTaxon) | is.na(CurrentTaxon) ~ Genus,
                        TRUE ~ str_remove(str_squish(str_remove(CurrentTaxon, 'cf.')), ' .*')),
      Species = case_when(Taxon == CurrentTaxon | grepl('None|Unknown',CurrentTaxon) | is.na(CurrentTaxon) ~ Species,
                          TRUE ~ str_remove(str_squish(str_remove(CurrentTaxon, 'cf.')), '.*? ')),
      OrigTaxon = case_when(Taxon == CurrentTaxon | grepl('None|Unknown',CurrentTaxon) | is.na(CurrentTaxon) ~ NA_character_,
                            TRUE ~ Taxon),
      Taxon = case_when(grepl('None|Unknown',CurrentTaxon) | is.na(CurrentTaxon) ~ Taxon,
                        TRUE ~ CurrentTaxon
      )) %>%
    select(-CurrentTaxon) %>%
    relocate(OrigTaxon, .after = Station)
  
  # standardize specific unknown Taxon cases
  df <- df %>%
    mutate(
      AlgalGroup = case_when(Taxon == 'Little Green Balls' ~ 'Unknown',
                             Taxon == 'Unknown cyanobacteria' ~ 'Cyanobacteria',
                             Taxon == 'Unknown centric diatom' ~ 'Centric Diatoms',
                             Taxon == 'Unknown green alga' ~ 'Green Algae',
                             Taxon == 'Unknown dinoflagellate' ~ 'Dinoflagellates',
                             Taxon == 'Unknown pennate diatom' ~ 'Pennate Diatoms',
                             Taxon == 'Unknown nanoflagellate' ~ 'Nanflagellates',
                             Taxon == 'Unknown euglenoid' ~ 'Euglenoids',
                             TRUE ~ AlgalGroup)
    ) 
  
  return(df)
}