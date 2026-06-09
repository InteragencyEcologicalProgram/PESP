format_tab <- nav_panel(
  'Format Data',
  br(),
  
  tags$div(
    class = 'tab-sections',
    
    accordion(
      open = FALSE,
      multiple = TRUE,
      
      accordion_panel(
        'Metadata',
        value = 'metadata_section',
        icon = icon('folder'),
        
        tags$div(
          class = 'task-accordion',
          
          accordion(
            open = FALSE,
            multiple = TRUE,
            
            accordion_panel(
              'Convert Timezone',
              value = 'timezone_panel',
              icon = icon('clock'),
              uiOutput('timezone_ui')
            ),
            
            accordion_panel(
              'Add Metadata Columns',
              value = 'metadata_panel',
              icon = icon('plus'),
              uiOutput('metadata_ui')
            ),
            
            accordion_panel(
              'Add Comment Columns',
              value = 'qc_panel',
              icon = icon('plus'),
              uiOutput('qc_ui')
            )
          )
        )
      ),
      
      accordion_panel(
        'Densities',
        value = 'densities_section',
        icon = icon('folder'),
        
        tags$div(
          class = 'task-accordion',
          
          accordion(
            open = FALSE,
            multiple = TRUE,
            
            accordion_panel(
              'Calculate Densities',
              value = 'densities_panel',
              icon = icon('calculator'),
              uiOutput('densities_ui')
            )
          )
        )
      ),
      
      accordion_panel(
        'Taxonomy',
        value = 'taxonomy_section',
        icon = icon('folder'),
        
        tags$div(
          class = 'task-accordion',
          
          accordion(
            open = FALSE,
            multiple = TRUE,
            
            accordion_panel(
              '1. Correct Taxon Typos',
              value = 'taxon_typos_panel',
              icon = icon('spell-check'),
              uiOutput('taxon_typos_ui')
            ),
            
            accordion_panel(
              '2. Standardize Unknowns',
              value = 'unknowns_panel',
              icon = icon('question'),
              uiOutput('unknowns_ui')
            ),
            
            accordion_panel(
              '3. Update Synonyms',
              value = 'synonyms_panel',
              icon = icon('arrows-rotate'),
              uiOutput('synonyms_ui')
            ),
            
            accordion_panel(
              '4. Add Higher Level Taxa Info',
              value = 'higher_taxa_panel',
              icon = icon('sitemap'),
              uiOutput('higher_taxa_ui')
            ),
            
            accordion_panel(
              '5. Combine Taxa Rows',
              value = 'combine_taxa_panel',
              icon = icon('compress'),
              uiOutput('combine_taxa_ui')
            )
          )
        )
      ),
      
      accordion_panel(
        'Subset Columns',
        value = 'column_panel',
        icon = icon('copy'),
        uiOutput('column_ui')
      ),
      
    )
  )
)