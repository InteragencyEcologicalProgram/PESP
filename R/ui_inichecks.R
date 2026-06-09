initial_checks_tab <- nav_panel(
  'Initial Checks',
  br(),
  p('Checks for any initial data quality issues.'),
  
  tags$div(
    class = 'tab-sections',
    
    accordion(
      open = FALSE,
      multiple = TRUE,
      
      accordion_panel(
        'Check Duplicate Rows',
        value = 'duplicate_panel',
        icon = icon('copy'),
        uiOutput('duplicate_ui')
      ),
      
      accordion_panel(
        'Check Date & Time',
        value = 'datetime_panel',
        icon = icon('calendar'),
        
        tags$div(
          class = 'task-accordion',
          
          accordion(
            open = FALSE,
            multiple = TRUE,
            
            accordion_panel(
              'Missing/Malformed Date & Time',
              value = 'missing_datetime_panel',
              icon = icon('calendar-xmark'),
              uiOutput('missing_datetime_ui')
            ),
            
            accordion_panel(
              'Extreme Times',
              value = 'extreme_times_panel',
              icon = icon('clock'),
              uiOutput('extreme_times_ui')
            )
          )
        )
      ),
      
      accordion_panel(
        'Confirm Stations',
        value = 'stations_panel',
        icon = icon('map-marker'),
        uiOutput('stations_ui')
      )
    )
  )
)