# ---- Format Data tab server logic ----

# ---- Convert Timezone ----

output$timezone_ui <- renderUI({
  if (is.null(merged_df())) {
    p('No merged data yet. Combine files first.')
  } else {
    schema  <- schemas[[current_groups()[1]]]
    preset  <- input$selected_preset
    applies <- isTRUE(schema$steps$convert_timezone[[preset]]$applies)
    
    if (!applies) return(not_applicable_alert())
    
    code    <- schema$code
    from_tz <- survey_metadata$Timezone[survey_metadata$Survey == code][1]
    
    if (from_tz == 'PDT') {
      p('✅ Data is already in PDT. No conversion needed.')
    } else {
      tagList(
        p(paste('Converting', from_tz, 'to PDT.')),
        actionButton('convert_tz_btn', 'Convert Timezone',
                     icon = icon('clock'), class = 'btn-primary'),
        uiOutput('timezone_result'),
        br(),
        DTOutput('timezone_preview')
      )
    }
  }
})

output$timezone_preview <- renderDT({
  req(merged_df())
  schema  <- schemas[[current_groups()[1]]]
  code    <- schema$code
  from_tz <- survey_metadata$Timezone[survey_metadata$Survey == code][1]
  
  if (from_tz == 'PDT') return(NULL)
  
  df <- merged_df()
  if (!all(c('Date', 'Time') %in% names(df))) return(NULL)
  
  if (from_tz == 'PST') {
    preview <- df
  } else {
    dates     <- as.Date(df$Date)
    year      <- as.integer(format(dates, '%Y'))
    dst_start <- as.Date(paste0(year, '-03-08'))
    dst_start <- dst_start + (7 - as.integer(format(dst_start, '%u'))) %% 7
    dst_end   <- as.Date(paste0(year, '-11-01'))
    dst_end   <- dst_end + (7 - as.integer(format(dst_end, '%u'))) %% 7
    in_dst    <- dates >= dst_start & dates < dst_end
    preview   <- df[!in_dst, ]
  }
  
  preview <- unique(preview[, c('Date', 'Time')])
  preview <- preview[order(preview$Date, preview$Time), ]
  
  datatable(preview, options = list(pageLength = 5, scrollX = TRUE),
            rownames = FALSE)
})

observeEvent(input$convert_tz_btn, {
  schema  <- schemas[[current_groups()[1]]]
  code    <- schema$code
  from_tz <- survey_metadata$Timezone[survey_metadata$Survey == code][1]
  merged_df(convert_timezone(merged_df(), from_tz))
  
  output$timezone_result <- renderUI({
    p('✅ Timezone converted to PDT.')
  })
})

# ---- Add metadata columns ----

output$metadata_ui <- renderUI({
  if (is.null(merged_df())) {
    p('No merged data yet. Combine files first.')
  } else {
    schema  <- schemas[[current_groups()[1]]]
    preset  <- input$selected_preset
    applies <- isTRUE(schema$steps$add_metadata[[preset]]$applies)
    
    if (!applies) return(not_applicable_alert())
    
    all_cols <- unique(unlist(schema$metadata_columns))
    defaults <- schema$metadata_columns[[preset]]
    
    tagList(
      p('Preview of metadata to be added. Contact Perry if updates are needed.'),
      checkboxGroupInput('metadata_cols', 'Columns to add:',
                         choices  = all_cols,
                         selected = defaults
      ),
      br(),
      DTOutput('metadata_preview'),
      br(),
      actionButton('add_metadata_btn', 'Add Metadata Columns',
                   icon = icon('plus'), class = 'btn-primary'),
      uiOutput('metadata_result')
    )
  }
})

output$metadata_preview <- renderDT({
  req(merged_df(), input$metadata_cols)
  schema <- schemas[[current_groups()[1]]]
  cols   <- input$metadata_cols
  code   <- schema$code
  meta   <- survey_metadata[survey_metadata$Survey == code, ]
  
  data_start <- min(as.Date(merged_df()$Date), na.rm = TRUE)
  data_end   <- max(as.Date(merged_df()$Date), na.rm = TRUE)
  
  preview <- data.frame(
    From = as.character(pmax(meta$`Starting Date`, data_start)),
    To   = as.character(pmin(meta$`Ending Date`,   data_end)),
    meta[, cols, drop = FALSE],
    check.names = FALSE
  )
  
  preview <- preview[preview$From <= preview$To, ]
  
  datatable(preview, options = list(scrollX = TRUE, dom = 't'), rownames = FALSE)
})

observeEvent(input$add_metadata_btn, {
  cols   <- input$metadata_cols
  schema <- schemas[[current_groups()[1]]]
  merged_df(add_metadata_cols(merged_df(), schema, survey_metadata, cols))
  
  output$metadata_result <- renderUI({
    p('✅ Metadata columns added.')
  })
})

# ---- Add Comment Columns ----

output$qc_ui <- renderUI({
  if (is.null(merged_df())) {
    p('No merged data yet. Combine files first.')
  } else {
    schema  <- schemas[[current_groups()[1]]]
    preset  <- input$selected_preset
    applies <- isTRUE(schema$steps$add_comment_cols[[preset]]$applies)
    
    if (!applies) return(not_applicable_alert())
    
    defaults <- schema$comment_col_defaults[[preset]]
    
    tagList(
      p('Only relevant for BSA. BSA has standard wording for QC issues, debris classifications, and miscellaneous notes. App converts these phrases into codes.'),
      p('If there are unknown phrases or typos, app will show them. Contact Perry if additional codes or keywords need to be added.'),
      checkboxGroupInput('qc_cols', 'Columns to add:',
                         choices  = c('QC', 'Debris', 'Notes'),
                         selected = defaults
      ),
      actionButton('add_qc_btn', 'Add Comment Columns',
                   icon = icon('plus'), class = 'btn-primary'),
      uiOutput('qc_result')
    )
  }
})

observeEvent(input$add_qc_btn, {
  df     <- merged_df()
  preset <- input$selected_preset
  cols   <- input$qc_cols
  
  if ('QC'     %in% cols) df <- add_qc_col(df)
  if ('Debris' %in% cols) df <- add_debris_col(df)
  if ('Notes'  %in% cols) df <- add_notes_col(df)
  
  if (preset == 'PESP') {
    comments  <- extract_unstandardized_comments(df, 'Comments', delimiter = '. ')
    unmatched <- attr(comments, 'log')$unmatched_comments
    unstandardized_comments(unmatched)
  }
  
  merged_df(df)
  
  output$qc_result <- renderUI({
    if (preset != 'PESP') {
      p('✅ Columns added.')
    } else {
      n <- if (is.null(unstandardized_comments())) 0 else nrow(unstandardized_comments())
      if (n == 0) {
        p('✅ No unstandardized comments found.')
      } else {
        tagList(
          p(paste('⚠️', n, 'unstandardized comment(s) found.')),
          downloadButton('download_comments_btn', 'Download Comments',
                         class = 'btn-secondary'),
          br(), br(),
          DTOutput('comments_table')
        )
      }
    }
  })
})

output$comments_table <- renderDT({
  req(unstandardized_comments())
  datatable(unstandardized_comments(), options = list(pageLength = 5, scrollX = TRUE))
})

output$download_comments_btn <- downloadHandler(
  filename = function() paste0('unstandardized_comments_', Sys.Date(), '.csv'),
  content  = function(file) write.csv(unstandardized_comments(), file, row.names = FALSE)
)

# ---- Correct Taxon Typos ----

output$taxon_typos_ui <- renderUI({
  if (is.null(merged_df())) {
    p('No merged data yet. Combine files first.')
  } else {
    schema  <- schemas[[current_groups()[1]]]
    preset  <- input$selected_preset
    applies <- isTRUE(schema$steps$correct_taxon_typos[[preset]]$applies)
    
    if (!applies) return(not_applicable_alert())
    
    tagList(
      p('Corrects known taxon name typos.'),
      actionButton('correct_typos_btn', 'Correct Taxon Typos',
                   icon = icon('spell-check'), class = 'btn-primary'),
      uiOutput('taxon_typos_result')
    )
  }
})

observeEvent(input$correct_typos_btn, {
  df          <- correct_taxon_typos(merged_df())
  corrections <- attr(df, 'log')$taxon_corrections
  attr(df, 'log') <- NULL
  merged_df(df)
  
  output$taxon_typos_result <- renderUI({
    n <- if (is.null(corrections)) 0 else nrow(corrections)
    if (n == 0) {
      tagList(br(), p('✅ No taxon typos found.'))
    } else {
      tagList(
        br(),
        p(paste('✅', n, 'taxon correction(s) made.')),
        downloadButton('download_typos_btn', 'Download Corrections',
                       class = 'btn-secondary'),
        br(), br(),
        DTOutput('taxon_typos_table')
      )
    }
  })
  
  output$taxon_typos_table <- renderDT({
    req(corrections)
    datatable(corrections, options = list(pageLength = 5, scrollX = TRUE),
              rownames = FALSE)
  })
  
  output$download_typos_btn <- downloadHandler(
    filename = function() paste0('taxon_corrections_', Sys.Date(), '.csv'),
    content  = function(file) write.csv(corrections, file, row.names = FALSE)
  )
})

# ---- Standardize Unknowns ----

output$unknowns_ui <- renderUI({
  if (is.null(merged_df())) {
    p('No merged data yet. Combine files first.')
  } else {
    schema   <- schemas[[current_groups()[1]]]
    preset   <- input$selected_preset
    cfg      <- schema$steps$standardize_unknowns[[preset]]
    applies  <- isTRUE(cfg$applies)
    
    if (!applies) return(not_applicable_alert())
    
    tagList(
      checkboxInput('std_sp',     'Standardize sp.',      value = isTRUE(cfg$std_sp)),
      checkboxInput('std_suffix', 'Standardize suffixes', value = isTRUE(cfg$std_suffix)),
      actionButton('standardize_unknowns_btn', 'Standardize Unknowns',
                   icon = icon('question'), class = 'btn-primary'),
      uiOutput('unknowns_result')
    )
  }
})

observeEvent(input$standardize_unknowns_btn, {
  df           <- clean_unknowns(merged_df(),
                                 std_sp     = isTRUE(input$std_sp),
                                 std_suffix = isTRUE(input$std_suffix))
  standardized <- attr(df, 'log')$clean_unknowns
  attr(df, 'log') <- NULL
  merged_df(df)
  
  output$unknowns_result <- renderUI({
    n <- if (is.null(standardized) || nrow(standardized) == 0) 0 else nrow(standardized)
    if (n == 0) {
      tagList(br(), p('✅ No unknowns found to standardize.'))
    } else {
      tagList(
        br(),
        p(paste('✅', n, 'unknown(s) standardized.')),
        downloadButton('download_unknowns_btn', 'Download Changes',
                       class = 'btn-secondary'),
        br(), br(),
        DTOutput('unknowns_table')
      )
    }
  })
  
  output$unknowns_table <- renderDT({
    req(standardized)
    datatable(standardized, options = list(pageLength = 5, scrollX = TRUE),
              rownames = FALSE)
  })
  
  output$download_unknowns_btn <- downloadHandler(
    filename = function() paste0('standardized_unknowns_', Sys.Date(), '.csv'),
    content  = function(file) write.csv(standardized, file, row.names = FALSE)
  )
})

# ---- Update Synonyms ----

output$synonyms_ui <- renderUI({
  if (is.null(merged_df())) {
    p('No merged data yet. Combine files first.')
  } else {
    schema  <- schemas[[current_groups()[1]]]
    preset  <- input$selected_preset
    applies <- isTRUE(schema$steps$update_synonyms[[preset]]$applies)
    
    if (!applies) return(not_applicable_alert())
    
    tagList(
      p('Updates taxon names to current accepted synonyms.'),
      p(tags$small(tags$em('Reference file version: 2025.01'))),
      actionButton('update_synonyms_btn', 'Update Synonyms',
                   icon = icon('arrows-rotate'), class = 'btn-primary'),
      uiOutput('synonyms_result')
    )
  }
})

observeEvent(input$update_synonyms_btn, {
  df       <- update_synonyms(merged_df())
  synonyms <- attr(df, 'log')$synonym_updates
  attr(df, 'log') <- NULL
  merged_df(df)
  
  output$synonyms_result <- renderUI({
    n <- if (is.null(synonyms) || nrow(synonyms) == 0) 0 else nrow(synonyms)
    if (n == 0) {
      tagList(br(), p('✅ No synonym updates found.'))
    } else {
      tagList(
        br(),
        p(paste('✅', n, 'synonym(s) updated.')),
        downloadButton('download_synonyms_btn', 'Download Changes',
                       class = 'btn-secondary'),
        br(), br(),
        DTOutput('synonyms_table')
      )
    }
  })
  
  output$synonyms_table <- renderDT({
    req(synonyms)
    datatable(synonyms, options = list(pageLength = 5, scrollX = TRUE),
              rownames = FALSE)
  })
  
  output$download_synonyms_btn <- downloadHandler(
    filename = function() paste0('updated_synonyms_', Sys.Date(), '.csv'),
    content  = function(file) write.csv(synonyms, file, row.names = FALSE)
  )
})

# ---- Add Higher Level Taxa Info ----

output$higher_taxa_ui <- renderUI({
  if (is.null(merged_df())) {
    p('No merged data yet. Combine files first.')
  } else {
    schema  <- schemas[[current_groups()[1]]]
    preset  <- input$selected_preset
    applies <- isTRUE(schema$steps$higher_lvl_taxa[[preset]]$applies)
    
    if (!applies) return(not_applicable_alert())
    
    tagList(
      p('Adds higher level taxonomic hierarchy data.'),
      p('If taxa are not found, contact Perry.'),
      p(tags$small(tags$em('Reference file version: 2025.01'))),
      actionButton('add_higher_taxa_btn', 'Add Higher Level Taxa',
                   icon = icon('sitemap'), class = 'btn-primary'),
      uiOutput('higher_taxa_result')
    )
  }
})

observeEvent(input$add_higher_taxa_btn, {
  schema   <- schemas[[current_groups()[1]]]
  preset   <- input$selected_preset
  std_type <- schema$steps$higher_lvl_taxa[[preset]]$std_type
  
  df <- higher_lvl_taxa(merged_df(), std_type = std_type)
  taxa_unmatched <- attr(df, 'log')$unmatched_taxa
  attr(df, 'log') <- NULL
  merged_df(df)
  
  output$higher_taxa_result <- renderUI({
    n <- if (is.null(taxa_unmatched) || nrow(taxa_unmatched) == 0) 0 else nrow(taxa_unmatched)
    if (n == 0) {
      tagList(br(), p('✅ All taxa matched to reference list.'))
    } else {
      tagList(
        br(),
        p(paste('⚠️', n, 'taxon/taxa not found in reference list; no higher level info added for these.')),
        downloadButton('download_higher_taxa_btn', 'Download Unmatched Taxa',
                       class = 'btn-secondary'),
        tags$a(
          href = paste0(
            'mailto:you@example.com',
            '?subject=PESP%3A%20Unmatched%20Taxa',
            '&body=', URLencode(paste0(
              'Unmatched taxa for ', schemas[[current_groups()[1]]]$survey, ':\n\n',
              'PureTaxon | Taxon\n',
              paste(paste(taxa_unmatched$PureTaxon, taxa_unmatched$Taxon, sep = ' | '), collapse = '\n')
            ))
          ),
          class = 'btn btn-info',
          icon('envelope'), ' Report Unmatched Taxa'
        ),
        br(), br(),
        DTOutput('higher_taxa_table')
      )
    }
  })
  
  output$higher_taxa_table <- renderDT({
    req(taxa_unmatched)
    datatable(taxa_unmatched, options = list(pageLength = 5, scrollX = TRUE),
              rownames = FALSE)
  })
  
  output$download_higher_taxa_btn <- downloadHandler(
    filename = function() paste0('unmatched_taxa_', Sys.Date(), '.csv'),
    content  = function(file) write.csv(taxa_unmatched, file, row.names = FALSE)
  )
})

# ---- Combine Taxa Rows ----

output$combine_taxa_ui <- renderUI({
  if (is.null(merged_df())) {
    p('No merged data yet. Combine files first.')
  } else {
    schema <- schemas[[current_groups()[1]]]
    preset <- input$selected_preset
    applies <- isTRUE(schema$steps$combine_taxa[[preset]]$applies)
    
    if (!applies) return(not_applicable_alert())
    
    key_cols <- schema$combine_key_cols
    
    tagList(
      h5('Step 1: Preview Rows to Combine'),
      p(
        'Shows rows that share the same sampling event (',
        tags$strong(paste(key_cols, collapse = ', ')),
        ') and will be merged in Step 2.'
      ),
      actionButton('check_distinct_btn', 'Preview',
                   icon = icon('search'), class = 'btn-secondary'),
      uiOutput('check_distinct_result'),
      hr(),
      h5('Step 2: Combine Rows'),
      p(
        'Sampling events are defined by the ',
        tags$strong(paste(key_cols, collapse = ', ')),
        ' columns. Rows sharing the same sampling event will be combined with counts summed,',
        ' and columns will be filtered to the survey\'s preset.'
      ),
      actionButton('combine_taxa_btn', 'Combine',
                   icon = icon('compress'), class = 'btn-primary'),
      uiOutput('combine_taxa_result')
    )
  }
})

observeEvent(input$check_distinct_btn, {
  df <- merged_df()
  
  df_full <- check_distinct(df, type = 'full')
  nondistinct_full <- attr(df_full, 'log')$nondistinct_allrows
  
  df_key <- check_distinct(df, type = 'key_cols')
  nondistinct_key  <- attr(df_key, 'log')$nondistinct_keyrows
  
  output$check_distinct_result <- renderUI({
    tagList(
      br(),
      h6('Full Row Duplicates (post-taxonomy):'),
      if (is.null(nondistinct_full) || nrow(nondistinct_full) == 0)
        p('✅ No full row duplicates found.')
      else
        tagList(
          p(paste('⚠️', nrow(nondistinct_full), 'row(s) will be merged.')),
          downloadButton('download_full_dupes_btn', 'Download Duplicates',
                         class = 'btn-secondary'),
          br(), br(),
          DTOutput('nondistinct_full_table')
        ),
      br(),
      h6('Key Column Duplicates (post-taxonomy):'),
      if (is.null(nondistinct_key) || nrow(nondistinct_key) == 0)
        p('✅ No key column duplicates found.')
      else
        tagList(
          p(paste('⚠️', nrow(nondistinct_key), 'row(s) will be merged.')),
          downloadButton('download_key_dupes_btn', 'Download Duplicates',
                         class = 'btn-secondary'),
          br(), br(),
          DTOutput('nondistinct_key_table')
        )
    )
  })
  
  output$nondistinct_full_table <- renderDT({
    req(nondistinct_full)
    datatable(nondistinct_full, options = list(pageLength = 5, scrollX = TRUE),
              rownames = FALSE)
  })
  
  output$nondistinct_key_table <- renderDT({
    req(nondistinct_key)
    datatable(nondistinct_key, options = list(pageLength = 5, scrollX = TRUE),
              rownames = FALSE)
  })
})

observeEvent(input$combine_taxa_btn, {
  preset           <- input$selected_preset
  schema           <- schemas[[current_groups()[1]]]
  key_cols         <- schema$combine_key_cols
  measurement_cols <- schema$combine_measurement_cols
  
  df       <- merged_df()
  df       <- combine_taxa(df, key_cols = key_cols, measurement_cols = measurement_cols)
  combined <- attr(df, 'log')$combined_taxa
  attr(df, 'log') <- NULL
  merged_df(df)
  
  output$combine_taxa_result <- renderUI({
    n <- if (is.null(combined) || nrow(combined) == 0) 0 else nrow(combined)
    if (n == 0) {
      tagList(br(), p('✅ No taxa rows combined.'))
    } else {
      tagList(
        br(),
        p(paste('✅', n, 'taxon group(s) combined.')),
        downloadButton('download_combined_taxa_btn', 'Download Combined Taxa Log',
                       class = 'btn-secondary'),
        br(), br(),
        DTOutput('combined_taxa_table')
      )
    }
  })
  
  output$combined_taxa_table <- renderDT({
    req(combined)
    datatable(combined, options = list(pageLength = 5, scrollX = TRUE),
              rownames = FALSE)
  })
  
  output$download_combined_taxa_btn <- downloadHandler(
    filename = function() paste0('combined_taxa_', Sys.Date(), '.csv'),
    content  = function(file) write.csv(combined, file, row.names = FALSE)
  )
})

# --- Calculate Densities ---

output$densities_ui <- renderUI({
  if (is.null(merged_df())) {
    p('No merged data yet. Combine files first.')
  } else {
    schema  <- schemas[[current_groups()[1]]]
    preset  <- input$selected_preset
    applies <- isTRUE(schema$steps$calc_densities[[preset]]$applies)
    
    if (!applies) return(not_applicable_alert())
    
    tagList(
      p('Calculates density values from unit abundance and cell count data.'),
      actionButton('calc_densities_btn', 'Calculate Densities',
                   icon = icon('calculator'), class = 'btn-primary'),
      uiOutput('densities_result')
    )
  }
})

observeEvent(input$calc_densities_btn, {
  df <- calc_data_bsa(merged_df(),
                      unit_col = 'UnitAbundance',
                      cell_col = 'CellCount')
  merged_df(df)
  
  output$densities_result <- renderUI({
    tagList(br(), p('✅ Densities calculated.'))
  })
})

# --- Subset Columns ---

output$column_ui <- renderUI({
  if (is.null(merged_df())) {
    p('No data, process files first.')
  } else {
    preset <- input$selected_preset
    schema <- schemas[[current_groups()[1]]]
    cols   <- schema$subset_cols[[preset]]
    
    if (is.null(cols)) return(not_applicable_alert())
    
    tagList(
      h5('Subset Columns'),
      p('Keeps only the columns defined for this survey/preset. Any missing columns will be added as blank (NA).'),
      p(tags$strong('Columns: '), paste(cols, collapse = ', ')),
      actionButton('subset_cols_btn', 'Subset Columns',
                   icon = icon('filter'), class = 'btn-secondary'),
      uiOutput('subset_cols_result')
    )
  }
})

observeEvent(input$subset_cols_btn, {
  preset <- input$selected_preset
  schema <- schemas[[current_groups()[1]]]
  cols   <- schema$subset_cols[[preset]]
  
  df <- merged_df()
  
  # Add missing columns as NA
  missing_cols <- setdiff(cols, names(df))
  for (col in missing_cols) df[[col]] <- NA
  
  # Subset and reorder
  df <- df[, cols, drop = FALSE]
  merged_df(df)
  
  output$subset_cols_result <- renderUI({
    tagList(
      br(),
      p(paste('✅ Subset to', length(cols), 'columns.')),
      if (length(missing_cols) > 0)
        p(paste('Added as blank:', paste(missing_cols, collapse = ', '))),
      br(),
      h6('Preview:'),
      DTOutput('subset_cols_table')
    )
  })
  
  output$subset_cols_table <- renderDT({
    req(merged_df())
    datatable(merged_df(), options = list(pageLength = 5, scrollX = TRUE),
              rownames = FALSE)
  })
})
