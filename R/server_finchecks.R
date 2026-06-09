# ---- Final checks tab server logic ----

# ---- Duplicate checks ----

output$final_duplicate_ui <- renderUI({
  if (is.null(merged_df())) {
    p('No data, process files first.')
  } else {
    schema  <- schemas[[current_groups()[1]]]
    preset  <- input$selected_preset
    applies <- isTRUE(schema$steps$duplicate_checks[[preset]]$applies)
    
    if (!applies) return(not_applicable_alert())
    
    key_cols <- schema$sampling_event_cols
    
    tagList(
      h5('Step 1: Full Row Duplicates'),
      p('Check for and remove rows where every column value is identical.'),
      actionButton('final_check_full_dupes_btn', 'Check Full Duplicates',
                   icon = icon('search'), class = 'btn-secondary'),
      uiOutput('final_full_dupes_result'),
      hr(),
      h5('Step 2: Sampling Event Duplicates'),
      p('Assumes each taxon is only recorded once per sampling event unless mulitple size classes were noted. Therefore, any duplicates indicate either:'),
      tags$ul(
        tags$li('data quality issues (remove bad data and reupload)'),
        tags$li('multiple size classes (merged during formatting phase)'),
        tags$li('special circumstance (contact developers to prevent incorrect merge)')
      ),
      p(
        'Sampling events are defined by the ',
        tags$strong(paste(key_cols, collapse = ', ')),
        ' columns.'
      ),
      actionButton('final_check_key_dupes_btn', 'Check Sampling Event Duplicates',
                   icon = icon('search'), class = 'btn-secondary'),
      uiOutput('final_key_dupes_result')
    )
  }
})

observeEvent(input$final_check_full_dupes_btn, {
  df      <- merged_df()
  is_dupe <- duplicated(df) | duplicated(df, fromLast = TRUE)
  full_dupes(df[is_dupe, ])
  
  output$final_full_dupes_result <- renderUI({
    n <- nrow(full_dupes())
    if (n == 0) {
      tagList(br(), p('✅ No full row duplicates found.'))
    } else {
      tagList(
        br(),
        p(paste('⚠️', n, 'full row duplicate(s) found.')),
        downloadButton('final_download_full_dupes_btn', 'Download Duplicates',
                       class = 'btn-secondary'),
        actionButton('final_remove_full_dupes_btn', 'Remove Duplicates',
                     icon = icon('trash'), class = 'btn-warning'),
        br(), br(),
        DTOutput('final_full_dupes_table')
      )
    }
  })
})

observeEvent(input$final_check_key_dupes_btn, {
  df       <- merged_df()
  key_cols <- schemas[[current_groups()[1]]]$sampling_event_cols
  is_dupe  <- duplicated(df[, key_cols]) | duplicated(df[, key_cols], fromLast = TRUE)
  key_dupes(df[is_dupe, ])
  
  output$final_key_dupes_result <- renderUI({
    n <- nrow(key_dupes())
    if (n == 0) {
      tagList(br(), p('✅ No sampling event duplicates found.'))
    } else {
      tagList(
        br(),
        p(paste('⚠️', n, 'sampling event duplicate(s) found.')),
        downloadButton('final_download_key_dupes_btn', 'Download Duplicates',
                       class = 'btn-secondary'),
        br(), br(),
        DTOutput('final_key_dupes_table')
      )
    }
  })
})

output$final_full_dupes_table <- renderDT({
  req(full_dupes())
  datatable(full_dupes(), options = list(pageLength = 5, scrollX = TRUE))
})

output$final_key_dupes_table <- renderDT({
  req(key_dupes())
  datatable(key_dupes(), options = list(pageLength = 5, scrollX = TRUE))
})

output$final_download_full_dupes_btn <- downloadHandler(
  filename = function() paste0('full_duplicates_', Sys.Date(), '.csv'),
  content  = function(file) write.csv(full_dupes(), file, row.names = FALSE)
)

output$final_download_key_dupes_btn <- downloadHandler(
  filename = function() paste0('sampling_event_duplicates_', Sys.Date(), '.csv'),
  content  = function(file) write.csv(key_dupes(), file, row.names = FALSE)
)

observeEvent(input$final_remove_full_dupes_btn, {
  merged_df(remove_duplicates(merged_df()))
  full_dupes(NULL)
  output$final_full_dupes_result <- renderUI({
    tagList(br(), p('✅ Full row duplicates removed.'))
  })
})

# ---- Missing Date & Time ----

output$final_missing_datetime_ui <- renderUI({
  if (is.null(merged_df())) {
    p('No data, process files first.')
  } else {
    schema  <- schemas[[current_groups()[1]]]
    preset  <- input$selected_preset
    applies <- isTRUE(schema$steps$missing_datetime_checks[[preset]]$applies)
    
    if (!applies) return(not_applicable_alert())
    
    tagList(
      p('Checks for missing or incorrectly formatted Date and Time values.'),
      actionButton('final_check_datetime_btn', 'Check Date & Time',
                   icon = icon('search'), class = 'btn-secondary'),
      uiOutput('final_datetime_result')
    )
  }
})

observeEvent(input$final_check_datetime_btn, {
  problems <- check_datetime(merged_df())
  datetime_problems(problems)
  
  output$final_datetime_result <- renderUI({
    n <- nrow(problems)
    if (n == 0) {
      tagList(br(), p('✅ No Date or Time issues found.'))
    } else {
      tagList(
        br(),
        p(paste('⚠️', n, 'row(s) with missing or malformed Date/Time.')),
        downloadButton('final_download_datetime_btn', 'Download Problematic Rows',
                       class = 'btn-secondary'),
        br(), br(),
        DTOutput('final_datetime_table')
      )
    }
  })
})

output$final_datetime_table <- renderDT({
  req(datetime_problems())
  datatable(datetime_problems(), options = list(pageLength = 5, scrollX = TRUE))
})

output$final_download_datetime_btn <- downloadHandler(
  filename = function() paste0('datetime_issues_', Sys.Date(), '.csv'),
  content  = function(file) write.csv(datetime_problems(), file, row.names = FALSE)
)

# ---- Extreme Times ----

output$final_extreme_times_ui <- renderUI({
  if (is.null(merged_df())) {
    p('No data, process files first.')
  } else {
    schema  <- schemas[[current_groups()[1]]]
    preset  <- input$selected_preset
    applies <- isTRUE(schema$steps$extreme_time_checks[[preset]]$applies)
    
    if (!applies) return(not_applicable_alert())
    
    tagList(
      p('Assumes data is in military time. Flags times before 05:00 or after 20:00, which are likely errors.'),
      p('0:00 may be used to indicate missing times.'),
      actionButton('final_check_extreme_btn', 'Check Extreme Times',
                   icon = icon('search'), class = 'btn-secondary'),
      uiOutput('final_extreme_result')
    )
  }
})

observeEvent(input$final_check_extreme_btn, {
  problems <- check_extreme_times(merged_df())
  extreme_problems(problems)
  
  output$final_extreme_result <- renderUI({
    n <- nrow(problems)
    if (n == 0) {
      tagList(br(), p('✅ No extreme times found.'))
    } else {
      tagList(
        br(),
        p(paste('⚠️', n, 'row(s) with extreme times.')),
        downloadButton('final_download_extreme_btn', 'Download Problematic Rows',
                       class = 'btn-secondary'),
        actionButton('final_convert_military_btn', 'Convert to Military Time',
                     icon = icon('clock'), class = 'btn-warning'),
        br(), br(),
        DTOutput('final_extreme_table')
      )
    }
  })
})

output$final_extreme_table <- renderDT({
  req(extreme_problems())
  datatable(extreme_problems(), options = list(pageLength = 5, scrollX = TRUE))
})

output$final_download_extreme_btn <- downloadHandler(
  filename = function() paste0('extreme_times_', Sys.Date(), '.csv'),
  content  = function(file) write.csv(extreme_problems(), file, row.names = FALSE)
)

observeEvent(input$final_convert_military_btn, {
  df_new  <- convert_to_military(merged_df())
  changed <- attr(df_new, 'military_converted')
  attr(df_new, 'military_converted') <- NULL
  merged_df(df_new)
  extreme_problems(NULL)
  
  output$final_extreme_result <- renderUI({
    tagList(
      br(),
      p(paste('✅ Times converted to military time.', length(changed), 'row(s) affected.')),
      if (length(changed) > 0)
        DTOutput('final_military_converted_table')
    )
  })
  
  output$final_military_converted_table <- renderDT({
    datatable(merged_df()[changed, ],
              options = list(pageLength = 5, scrollX = TRUE), rownames = FALSE)
  })
})

# ---- Missing data check ----

output$final_na_ui <- renderUI({
  if (is.null(merged_df())) {
    p('No data, process files first.')
  } else {
    schema  <- schemas[[current_groups()[1]]]
    preset  <- input$selected_preset
    applies <- isTRUE(schema$steps$na_checks[[preset]]$applies)
    
    if (!applies) return(not_applicable_alert())
    
    tagList(
      h5('Missing Data'),
      p('Check for missing values across all columns. Required columns should have no NAs; optional columns may have expected missingness.'),
      actionButton('final_check_nas_btn', 'Check Missing Data',
                   icon = icon('search'), class = 'btn-secondary'),
      uiOutput('final_na_result')
    )
  }
})

observeEvent(input$final_check_nas_btn, {
  df     <- merged_df()
  df     <- check_nas(df)
  na_log <- attr(df, 'log')$na_check
  na_data(na_log)
  
  output$final_na_result <- renderUI({
    if (is.null(na_log) || nrow(na_log) == 0) {
      tagList(br(), p('✅ No missing values found.'))
    } else {
      tagList(
        br(),
        p(paste('⚠️', nrow(na_log), 'column(s) with missing values found.')),
        downloadButton('final_download_nas_btn', 'Download Report',
                       class = 'btn-secondary'),
        br(), br(),
        DTOutput('final_na_table')
      )
    }
  })
})

output$final_na_table <- renderDT({
  req(na_data())
  datatable(na_data(), options = list(pageLength = 5, scrollX = TRUE))
})

output$final_download_nas_btn <- downloadHandler(
  filename = function() paste0('missing_data_', Sys.Date(), '.csv'),
  content  = function(file) write.csv(na_data(), file, row.names = FALSE)
)