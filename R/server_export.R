# --- Export tab server logic ---

output$export_ui <- renderUI({
  if (is.null(merged_df())) {
    p('No data, process files first.')
  } else {
    survey <- input$selected_survey
    tagList(
      p(paste(nrow(merged_df()), 'rows ready to export.')),
      downloadButton('export_btn', 'Download Data', icon = icon('download'),
                     class = 'btn-primary')
    )
  }
})

output$export_btn <- downloadHandler(
  filename = function() {
    survey <- input$selected_survey
    date   <- format(Sys.Date(), '%Y-%m-%d')
    paste0(survey, '_', date, '.csv')
  },
  content = function(file) {
    write.csv(merged_df(), file, row.names = FALSE)
  }
)