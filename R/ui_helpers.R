app_css <- tags$head(
  tags$style(
    HTML("
      .selectize-dropdown .selected {
        background-color: transparent;
        color: inherit;
      }

      .selectize-dropdown .option:hover {
        color: inherit;
      }

      .bslib-sidebar-layout > .collapse-toggle {
        display: none;
      }

      .sidebar-content {
        padding-top: 0 !important;
      }

      .sidebar-content .form-group:first-child {
        margin-top: 0.75rem;
      }

      .shiny-html-output p {
        margin-top: 0.75rem;
      }

      /* data tables */
      .dataTables_wrapper {
        border: 1px solid #8f9aa3;
        border-radius: 6px;
        padding: 0.75rem;
      }

      /* outer section blocks */
      .tab-sections > .accordion > .accordion-item {
        margin-bottom: 1rem;
        border: 1px solid #6c757d;
        border-radius: 6px;
        overflow: hidden;
      }

      /* outer section header: collapsed */
      .tab-sections > .accordion > .accordion-item > .accordion-header > .accordion-button.collapsed {
        background-color: #d6dbe0;
        color: #000;
        padding: 0.9rem 1rem;
      }

      /* outer section header: open */
      .tab-sections > .accordion > .accordion-item > .accordion-header > .accordion-button:not(.collapsed) {
        background-color: #495057;
        color: #fff;
        box-shadow: none;
        padding: 0.9rem 1rem;
      }

      .tab-sections > .accordion > .accordion-item > .accordion-header > .accordion-button:not(.collapsed)::after {
        filter: invert(1);
      }

      .tab-sections > .accordion > .accordion-item > .accordion-collapse > .accordion-body {
        padding: 0.5rem;
      }

      /* inner task accordions */
      .task-accordion > .accordion > .accordion-item {
        margin-bottom: 0.6rem;
        border: 1px solid #8f9aa3;
        border-radius: 6px;
        overflow: hidden;
      }

      /* inner task header: collapsed */
      .task-accordion > .accordion > .accordion-item > .accordion-header > .accordion-button.collapsed {
        background-color: #e4e8eb;
        color: #000;
        padding: 0.75rem 1rem;
      }

      /* inner task header: open */
      .task-accordion > .accordion > .accordion-item > .accordion-header > .accordion-button:not(.collapsed) {
        background-color: #c7ced4;
        color: #000;
        box-shadow: none;
        padding: 0.75rem 1rem;
      }

      .task-accordion > .accordion > .accordion-item > .accordion-collapse > .accordion-body {
        padding: 1rem 1.25rem;
      }
    ")
  )
)