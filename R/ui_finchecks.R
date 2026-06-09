final_checks_tab <- nav_panel(
  'Final Checks',
  br(),
  p('Checks for any persistent or introduced data quality issues.'),
  
  tags$div(
    class = 'tab-sections',
    
    accordion(
      open = FALSE,
      multiple = TRUE,
      
      accordion_panel(
        'Check Duplicate Rows',
        value = 'final_duplicate_panel',
        icon = icon('copy'),
        uiOutput('final_duplicate_ui')
      ),
      
      accordion_panel(
        'Check Date & Time',
        value = 'final_datetime_panel',
        icon = icon('calendar'),
        
        tags$div(
          class = 'task-accordion',
          
          accordion(
            open = FALSE,
            multiple = TRUE,
            
            accordion_panel(
              'Missing/Malformed Date & Time',
              value = 'final_missing_datetime_panel',
              icon = icon('calendar-xmark'),
              uiOutput('final_missing_datetime_ui')
            ),
            
            accordion_panel(
              'Extreme Times',
              value = 'final_extreme_times_panel',
              icon = icon('clock'),
              uiOutput('final_extreme_times_ui')
            )
          )
        )
      ),
      
      accordion_panel(
        'Check Missing Data',
        value = 'final_na_panel',
        icon = icon('check'),
        uiOutput('final_na_ui')
      )
    )
  )
)