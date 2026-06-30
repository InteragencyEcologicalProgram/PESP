# --- Group-specific formatting ---
# Dispatch function: routes to the correct formatter by group name.

format_data <- function(df, survey) {
  switch(survey,
         'DWR-YBFMP (BSA)' = format_dwr_yolo_bsa(df),
         'USGS-BP (BSA)' = format_usgs_bp_bsa(df),
         'DWR-DEMP (BSA)' = format_dwr_demp_bsa(df),
         'testing' = format_dwr_yolo_bsa(df),
         df
  )
}

format_dwr_yolo_bsa <- function(df) {
  # fix date
  if ('Date' %in% names(df))
    df$Date <- format(as.Date(as.numeric(df$Date), origin = '1899-12-30'), '%Y-%m-%d')
  
  # fix time
  if ('Time' %in% names(df))
    df$Time <- format(as.POSIXct(df$Time), '%H:%M')
  
  # remove Dimension and all Measurement columns
  drop_cols <- grep('^(Dimension|Measure)', names(df), value = TRUE)
  df <- df[, !names(df) %in% drop_cols, drop = FALSE]
  
  # remove rows where all values are NA
  df <- df[rowSums(!is.na(df)) > 0, ]
  
  # remove old taxa info to avoid conflicts
  df <- remove_taxa_info(df)
  
  df
}

format_usgs_bp_bsa <- function(df) {
  # fix date
  if ('Date' %in% names(df))
    df$Date <- format(as.Date(df$Date, format = '%d/%m/%Y'), '%Y-%m-%d')
 
  # remove rows where all values are NA
  df <- df[rowSums(!is.na(df)) > 0, ]
  
  # remove old taxa info to avoid conflicts
  df <- remove_taxa_info(df)
  
  df
}

format_dwr_demp_bsa <- function(df) {
  # fix date
  if ('Date' %in% names(df))
    df$Date <- format(as.Date(as.numeric(df$Date), origin = '1899-12-30'), '%Y-%m-%d')
  
  # fix time
  if ('Time' %in% names(df))
    df$Time <- format(as.POSIXct(df$Time), '%H:%M')
  
  # remove Dimension and all Measurement columns
  drop_cols <- grep('^(Dimension|Measure)', names(df), value = TRUE)
  df <- df[, !names(df) %in% drop_cols, drop = FALSE]
  
  # remove rows where all values are NA
  df <- df[rowSums(!is.na(df)) > 0, ]
  
  # remove old taxa info to avoid conflicts
  df <- remove_taxa_info(df)
  
  df
}

# --- Date/Time checks ---

check_datetime <- function(df) {
  problems <- data.frame()
  
  if ('Date' %in% names(df)) {
    bad_date <- df[is.na(df$Date) | is.na(as.Date(df$Date, format = '%Y-%m-%d')), ]
    if (nrow(bad_date) > 0) {
      bad_date$issue <- 'Bad/missing Date'
      problems <- bind_rows(problems, bad_date)
    }
  }
  
  if ('Time' %in% names(df)) {
    bad_time <- df[is.na(df$Time) | !grepl('^([01]?[0-9]|2[0-3]):[0-5][0-9]$', df$Time), ]
    if (nrow(bad_time) > 0) {
      bad_time$issue <- 'Bad/missing Time'
      problems <- bind_rows(problems, bad_time)
    }
  }
  
  problems
}

check_extreme_times <- function(df) {
  if (!'Time' %in% names(df)) return(data.frame())
  
  valid_times <- df[!is.na(df$Time) & grepl('^([01]?[0-9]|2[0-3]):[0-5][0-9]$', df$Time), ]
  hours <- as.integer(sub(':.*', '', valid_times$Time))
  valid_times[hours < 5 | hours >= 20, ]
}

convert_to_military <- function(df) {
  if (!'Time' %in% names(df)) return(df)
  
  original <- df$Time
  hours    <- as.integer(sub(':.*', '', df$Time))
  mins     <- sub('.*:', '', df$Time)
  
  # 0:00 -> NA
  df$Time <- ifelse(!is.na(hours) & hours == 0, NA, df$Time)
  
  # before 5 -> add 12
  needs_fix <- !is.na(hours) & hours > 0 & hours < 5
  df$Time   <- ifelse(needs_fix, sprintf('%02d:%s', hours + 12, mins), df$Time)
  
  # return df with a changed attribute to track converted rows
  attr(df, 'military_converted') <- which(needs_fix | (!is.na(original) & is.na(df$Time)))
  df
}

# --- Timezone conversion ---

convert_timezone <- function(df, from_tz) {
  if (!all(c('Time', 'Date') %in% names(df))) return(df)
  
  dates <- as.Date(df$Date)
  times <- df$Time
  
  # determine offset per row
  if (from_tz == 'PDT') {
    offsets <- rep(0, nrow(df))
  } else if (from_tz == 'PST') {
    offsets <- rep(1, nrow(df))
  } else {
    # PST/PDT — vectorized DST check
    year      <- as.integer(format(dates, '%Y'))
    dst_start <- as.Date(paste0(year, '-03-08'))
    dst_start <- dst_start + (7 - as.integer(format(dst_start, '%u'))) %% 7
    dst_end   <- as.Date(paste0(year, '-11-01'))
    dst_end   <- dst_end + (7 - as.integer(format(dst_end, '%u'))) %% 7
    in_dst    <- dates >= dst_start & dates < dst_end
    offsets   <- ifelse(in_dst, 0, 1)
  }
  
  # apply offsets vectorized
  valid     <- !is.na(times) & grepl('^([01]?[0-9]|2[0-3]):[0-5][0-9]$', times)
  hours     <- as.integer(sub(':.*', '', times))
  mins      <- sub('.*:', '', times)
  new_hours <- (hours + offsets) %% 24
  df$Time   <- ifelse(valid, sprintf('%02d:%s', new_hours, mins), times)
  
  df
}

# --- Duplicate checking ---

check_duplicates <- function(df) {
  sum(duplicated(df))
}

remove_duplicates <- function(df) {
  df[!duplicated(df), ]
}

check_key_duplicates <- function(df, key_cols) {
  sum(duplicated(df[, key_cols]))
}

remove_key_duplicates <- function(df, key_cols) {
  df[!duplicated(df[, key_cols]), ]
}

# --- Metadata column joining ---

add_metadata_cols <- function(df, schema, survey_metadata, cols) {
  code <- schema$code
  meta <- survey_metadata[survey_metadata$Survey == code, ]
  
  df$Date <- as.Date(df$Date)
  for (col in cols) df[[col]] <- NA_character_
  
  for (i in seq_len(nrow(df))) {
    date  <- df$Date[i]
    match <- meta[!is.na(meta$`Starting Date`) &
                    meta$`Starting Date` <= date &
                    meta$`Ending Date`   >= date, ]
    if (nrow(match) >= 1) {
      for (col in cols) df[[col]][i] <- as.character(match[[col]][1])
    }
  }
  df
}