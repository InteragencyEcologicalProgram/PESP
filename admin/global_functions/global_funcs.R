
# Useful Functions --------------------------------------------------------

# # create object 'not in' for logic purposes

'%!in%' <- function(x,y)!('%in%'(x,y))

# # quietly read in csv

read_quiet_csv <- function(fp){
  df <- suppressWarnings(readr::read_csv(fp, show_col_types = FALSE))
  
  return(df)
}

# # absolute path to PESP data

abs_data_path <- function(fp_rel = NULL) {
  fp_full <- 'California Department of Water Resources/Phytoplankton synthesis - Documents/'
  
  if (is.null(fp_rel)) {
    fp_abs <- normalizePath(file.path(Sys.getenv('USERPROFILE'), fp_full))
  } else {
    fp_abs <- normalizePath(file.path(Sys.getenv('USERPROFILE'), fp_full, fp_rel))
  }
  
  return(fp_abs)
}

# # read in metadata file

read_meta_file <- function(program_name){
  df <-
    readxl::read_xlsx(abs_data_path('Metadata/Phytoplankton Metadata.xlsx'), skip = 3)
  
  df <- df %>%
    subset(Program == program_name) 
  
  df$`Ending Date`[is.na(df$`Ending Date`)] <- Sys.Date()
  
  return(df)
}

# # read in EDI

get_edi_file = function(pkg_id, fnames){
  # get revision
  revision_url = glue::glue('https://pasta.lternet.edu/package/eml/edi/{pkg_id}')
  all_revisions = readLines(revision_url, warn = FALSE) 
  latest_revision = tail(all_revisions, 1)
  
  # get entities 
  pkg_url = glue::glue('https://pasta.lternet.edu/package/data/eml/edi/{pkg_id}/{latest_revision}')
  all_entities = readLines(pkg_url, warn = FALSE)
  name_urls = glue::glue('https://pasta.lternet.edu/package/name/eml/edi/{pkg_id}/{latest_revision}/{all_entities}')
  names(all_entities) = purrr::map_chr(name_urls, readLines, warn = FALSE)
  
  # select entities that match fnames
  fname_regex = stringr::str_c(glue::glue('({fnames})'), collapse = '|')
  included_entities = all_entities[stringr::str_detect(names(all_entities), fname_regex)]
  if(length(included_entities) != length(fnames)){
    stop('Not all specified filenames are included in package')
  }
  # download data
  dfs = purrr::map(glue::glue('https://portal.edirepository.org/nis/dataviewer?packageid=edi.{pkg_id}.{latest_revision}&entityid={included_entities}'),
                   readr::read_csv, guess_max = 1000000)
  names(dfs) = names(included_entities)
  dfs
}

# Modify Data Frame --------------------------------------------------------

# # add values from metadata file column

from_meta <- function(df, meta_df, column) {
  meta_df <- meta_df %>%
    mutate(start = `Starting Date`,
           end = if_else(is.na(`Ending Date`), Sys.Date(), `Ending Date`)) %>%
    mutate(Date = map2(start, end, ~ seq(from = .x, to = .y, by = 'day'))) %>%
    unnest(cols = Date) %>%
    select(Date, all_of(column))
  
  df_export <- left_join(df, meta_df, by = 'Date')
  
  return(df_export)
}

# # create a 'quality check' column based on a comment column

add_qc_col <- function(df, comment_col){
  comment_col <- ensym(comment_col) 
  
  df <- df %>%
    mutate(
      QC_1 = case_when(grepl('delete|cross contamination', Comments, ignore.case = TRUE) ~ 'BadData'),
      QC_2 = case_when(grepl('did not reach|cannot meet tally|cannot meet natural unit', Comments, ignore.case = TRUE) ~ 'TallyNotMet'),
      QC_3 = case_when(grepl('degraded', Comments, ignore.case = TRUE) ~ 'Degraded'),
      QC_4 = case_when(grepl('poor preservation|poorly preserved|weak preservation|weakly preserved|fungus', Comments, ignore.case = TRUE) ~ 'PoorlyPreserved'),
      QC_5 = case_when(grepl('obscured', Comments, ignore.case = TRUE) ~ 'Obscured'),
      QC_6 = case_when(grepl('many broken diatoms', Comments, ignore.case = TRUE) ~ 'BrokenDiatoms')
    ) %>%
    unite(QualityCheck, starts_with('QC'), remove = TRUE, na.rm = TRUE, sep = ' ')
  
  df$QualityCheck[df$QualityCheck == ''] <- 'Good'
  
  return(df)
}

# # create a debris column based on a comment column

add_debris_col <- function(df, comment_col){
  comment_col <- ensym(comment_col) 
  
  df <- df %>%
    mutate(
      Db_1 = case_when(
        grepl('high detritus|high sediment|heavy detritus|heavy sediment', Comments, ignore.case = TRUE) ~ 'high',
        grepl('moderate detritus|moderate sediment', Comments, ignore.case = TRUE) ~ 'moderate',
        grepl('low detritus|low sediment|light detritus|light sediment', Comments, ignore.case = TRUE) ~ 'low')
    ) %>%
    unite(Debris, starts_with('Db'), remove = TRUE, na.rm = TRUE, sep = ' ')
}

# # standardize spp. to sp.

clean_sp <- function(df){
  df$Taxon <- stringr::str_replace_all(df$Taxon, 'spp.', 'sp.')
  df$Species <- stringr::str_replace_all(df$Species, 'spp.', 'sp.')
  df$Taxon <- str_replace(df$Taxon, 'sp\\..*', 'sp.')
  df$Species <- str_replace(df$Species, 'sp\\..*', 'sp.')
  
  return(df)
}


# Taxa Related Functions --------------------------------------------------

# # add in higher lvl taxa

higher_lvl_taxa <- function(df, after_col){
  # data frame containing Tiffany Brown's standardized phyto taxa csv
  df_syn <- read_quiet_csv('admin/global_data/phyto_classifications.csv') %>%
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
        TRUE ~ Taxon))
  
  # create column for merging the taxon
  df <- df %>% mutate(PureTaxon = Taxon)
  
  df <- df %>%
    mutate(PureTaxon = gsub('cf\\. ', '', PureTaxon),
           PureTaxon = gsub(' var\\..*', '', PureTaxon))
  
  # add higher level taxa to data df (joined df)
  df_joined <- df %>%
    left_join(df_syn, by = 'PureTaxon') %>%
    select(-c(ends_with('.y'), ends_with('.x'), 'PureTaxon')) %>%
    relocate(c(Taxon, Kingdom, Phylum, Class, AlgalGroup), .after = all_of(after_col)) %>%
    relocate(c(Genus, Species), .after = AlgalGroup)
  
  return(df_joined)
}


# # add metadata cols

add_meta_col <- function(df, program, col_name){
  # read in metadata sheet
  df_meta <- read_meta_file(program)
  
  df <- from_meta(df, df_meta, {{ col_name }})
}


# # update synonyms

update_synonyms <- function(df) {
  # map the current/original taxon
  synonym_map <- setNames(as.character(df$CurrentTaxon), df$Taxon)
  
  # follow logical sequence to newest taxon
  newest_taxon <- function(taxon) {
    while (!is.na(synonym_map[taxon]) && synonym_map[taxon] != "None") {
      taxon <- synonym_map[taxon]
    }
    return(taxon)
  }
  
  df <- df %>%
    mutate(
      OrigTaxon = Taxon,
      Taxon = sapply(Taxon, newest_taxon)
    ) %>%
    mutate(OrigTaxon = ifelse(OrigTaxon == Taxon, NA, OrigTaxon)) %>%
    select(-CurrentTaxon) %>% 
    relocate(OrigTaxon, .before = Taxon) 
  
  return(df)
}

# # standardize unknowns

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