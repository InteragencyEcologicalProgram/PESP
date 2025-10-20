# update location abbreviations
update_locs_frp <- function(df) {
  df_locs <- read_quiet_csv(abs_pesp_path('Groups/CDFW-FRP/Metadata/FRP_location_abbrevs.csv'))
  
  # left join by Location
  df_joined <- df %>%
    left_join(df_locs, by = 'Location')
  
  # check for unmatched Locations
  missing_locs <- setdiff(unique(df$Location), unique(df_locs$Location))
  
  if (length(missing_locs) > 0) {
    message('missing locations: \n  ',
            paste(missing_locs, collapse = ', '))
  } else {
    message('all locations matched successfully.')
  }
  
  return(df_joined)
}