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
        uiOutput('datetime_ui')
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

final_checks_tab <- nav_panel(
  'Final Checks',
  br(),
  
  tags$div(
    class = 'tab-sections',
    
    accordion(
      open = FALSE,
      multiple = TRUE,
      
      accordion_panel(
        'Final Checks',
        value = 'final_checks_panel',
        icon = icon('check'),
        p('Final checks will go here.')
      )
    )
  )
)