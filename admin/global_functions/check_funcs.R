#' Check station names
#'
#' Checks for errors/typos in data frame's station names
#'
#' @param col_data <- data column containing station names
#'
#' @param col_check <- data column containing official station list names
#' 
check_stations <- function(col_data, col_check){
  
  ls_ex_stations <- unique(col_data)[!(unique(col_data) %in% c(col_check))]
  
  df_ex_stations <- data.frame('extra stations' = ls_ex_stations)
  
  if (length(ls_ex_stations) > 0){
    warning(glue('Station(s) found that are not in the official list: {toString(ls_ex_stations)}.\nEither update official list or fix station name(s).'))
  } else {
    message('All station names in given station list.')
  }
  
  return(df_ex_stations)
}

# Check Distinct
check_distinct <- function(df, return_df = FALSE){
  row_check = identical(nrow(df), nrow(distinct(df)))
  
  if (!isTRUE(row_check)){
    warning('non-distinct rows in data frame')
    
    if (isTRUE(return_df)){
      df_dupes <- df_joined[duplicated(df_joined),]
      
      return(df_dupes)          
    }
    
  } else {
    message('All rows are unique')
  }
}

#' Check higher level taxa
#'
#' Checks for taxa without higher level classification info
#'
#' @param df <- df to check
#' 
check_higher_taxa <- function(df){
  df_error_check <- df %>% select(c('Taxon':'Species'))
  
  check <- df_error_check %>% subset(is.na(AlgalGroup) | is.na(Class) | is.na(Phylum) | is.na(Kingdom) | is.na(Genus) | is.na(Species))
  check <- check %>% mutate(Taxon = gsub('cf\\. ', '', Taxon))
  check <- check[!duplicated(check),]
  check <- check %>% arrange(Taxon)  
  
  if (nrow(check) > 0){
    warning(glue('Taxon missing higher level classifications:\n{toString(unique(check$Taxon)\n)}\nEither update official list or fix name(s)'))
  } else {
    message('All higher level classifications added.')
  }
  
  return(check)
}

check_synonyms <- function(df){
  changed_taxon <- unique(df_syn$Taxon[df_syn$CurrentTaxon != 'None'])
  
  multigen <- changed_taxon[changed_taxon %in% unique(df_syn$CurrentTaxon)]
  
  df_output <- data.frame(multigen) %>%
    mutate(Type = 'multigen')
  
  if (length(multigen) > 0){
    warning_one <- glue('Warning: multi-generational taxon names in synonym dataframe: {toString(multigen)}.')
  } else {
    warning_one <- ''
  }
  
  df_error_check <- df %>% select(c('Taxon':'Species'))
  
  syn_check <- df_error_check %>% subset(is.na(Taxon) | Taxon == 'Unknown')
  syn_check <- syn_check[!duplicated(syn_check),]
  syn_check <- syn_check %>% arrange(Taxon)
  
  if (nrow(syn_check) > 0){
    message(glue('{warning_one} \n Warning: Taxon missing synonym data: {toString(unique(syn_check$Taxon)\n)}\n Either update official list or fix name(s)'))
  } else {
    message(glue('{warning_one} \n All synonyms added.'))
  }
  
  syn_check <- syn_check %>%
    mutate(Type = 'missing syn')
  
  df_output <- full_join(df_output, syn_check, by = 'Type')
  
  return(df_output)
}
