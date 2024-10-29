
# Check Variables ---------------------------------------------------------

# # check station names

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

# # check methods

check_methods <- function(df){
  ls_methods <- unique(df$SamplingMethod)
  
  message(message(glue::glue('Currect collection type methods: {toString(ls_methods)}')))
}

# # check distinct rows

check_distinct <- function(df, return_df = FALSE){
  row_check = identical(nrow(df), nrow(distinct(df)))
  
  if (!isTRUE(row_check)){
    warning('non-distinct rows in data frame')
    
    if (isTRUE(return_df)){
      df_dupes <- df[duplicated(df),]
      
      return(df_dupes)          
    }
    
  } else {
    message('All rows are unique')
  }
}

# Check Taxa --------------------------------------------------------------

# # check higher lvl taxa

check_higher_taxa <- function(df){
  df_error_check <- df %>% select(c('Taxon':'Species'))
  
  check <- df_error_check %>% subset(is.na(AlgalGroup) | is.na(Class) | is.na(Phylum) | is.na(Kingdom) | is.na(Genus) | is.na(Species))
  check <- check %>% mutate(Taxon = gsub('cf\\. ', '', Taxon),
                            Taxon = gsub(' var\\..*', '', Taxon))
  check <- check[!duplicated(check),]
  check <- check %>% arrange(Taxon)  
  
  if (nrow(check) > 0){
    warning(glue('Taxon missing higher level classifications:\n{toString(unique(check$Taxon)\n)}\nEither update official list or fix name(s)'))
  } else {
    message('All higher level classifications added.')
  }
  
  return(check)
}

# # check synonyms

check_synonyms <- function(df){
  df_syn <- read_quiet_csv('admin/global_data/phyto_classifications.csv') %>%
    select(c('Kingdom':'AlgalGroup','Taxon','CurrentTaxon'))
  
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

# Check Plots -------------------------------------------------------------

# # Plot NMDS
create_nmds <- function(df, group_var, nmds_var, factor_var = NULL, show_legend = TRUE, color_palette = NULL) {
  set.seed(42)

  if (!is.null(factor_var)) {
    group_var <- rlang::sym(group_var)
    nmds_var <- rlang::sym(nmds_var)
    factor_var <- rlang::sym(factor_var)
    
    df_cells <- df %>%
      select(!!group_var, !!factor_var, Taxon, !!nmds_var) %>%
      group_by(!!group_var, !!factor_var, Taxon) %>%
      reframe(!!nmds_var := mean(!!nmds_var, na.rm = TRUE)) %>%
      pivot_wider(names_from = Taxon, values_from = !!nmds_var)
    
    com <- df_cells %>%
      select(-!!group_var, -!!factor_var)
    com[is.na(com)] <- 0
    m_com <- as.matrix(com)
    
  } else {
    factor_var <- rlang::sym(group_var)
    nmds_var <- rlang::sym(nmds_var)
    
    df_cells <- df %>%
      select(!!factor_var, Taxon, !!nmds_var) %>%
      group_by(!!factor_var, Taxon) %>%
      reframe(!!nmds_var := mean(!!nmds_var, na.rm = TRUE)) %>%
      pivot_wider(names_from = Taxon, values_from = !!nmds_var)
    
    com <- df_cells %>%
      select(-!!factor_var)
    com[is.na(com)] <- 0
    m_com <- as.matrix(com)
  }
  
  nmds <- metaMDS(m_com, distance = 'bray')
  df_nmds <- as.data.frame(scores(nmds)$sites)
  
  df_nmds <- df_nmds %>%
    mutate(!!factor_var := df_cells %>% pull(!!factor_var)) %>%
    mutate(!!factor_var := as.factor(!!factor_var))
  
  plt_nmds <- ggplot(df_nmds, aes(x = NMDS1, y = NMDS2, group = !!factor_var)) + 
    geom_point(size = 4, shape = 21, color = '#000000', aes(fill = !!factor_var)) +
    theme_bw()
  
  if (!is.null(color_palette)) {
    plt_nmds <- plt_nmds + scale_fill_manual(values = color_palette)
  }
  
  if (!show_legend) {
    plt_nmds <- plt_nmds + theme(legend.position = 'none')
  }
    
  return(list(
    nmds_df = df_nmds,
    plot = plt_nmds
  ))
}








