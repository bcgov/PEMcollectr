shiny::navbarPage(
  windowTitle = 'PEMcollectr',
  title = shiny::tags$img(src = 'gov_bc_logo.svg', height = 35),
  header = shiny::tags$link(rel = 'stylesheet', type = 'text/css',
    href = 'styles.css'),
  theme = bcgov_theme,
  inverse = TRUE,
  shiny::tabPanel(title = 'Data Submission',
    shiny::fluidRow(
      shiny::column(width = 3,
        card(title = shiny::tags$strong('Data Validation'),
          geomUploadUi('geomUpload'),
          shiny::tags$div(style =
              'height: 0px; margin-top: 15px; margin-bottom: 5px;'),
          dbWriteUi('dbWrite'),
          class = 'bg-secondary mb-3'),
        card(title = shiny::tags$strong('Transect Pair Validation'),
          validatePairsUi('validatePairs'),
          class = 'bg-secondary mb-3')
      ),
      shiny::column(width = 9,
        card(title = shiny::tags$strong('Validation Results'),
          validateTableUi('validateTable'),
          class = 'bg-secondary mb-3'
        )
      )
    )
  ),
  shiny::tabPanel(title = 'Map',
    shiny::fluidRow(
      shiny::column(width = 12,
        card(title = shiny::tags$strong('Data Collection Map'),
          mapUi('dcMap'),
          class = 'bg-secondary mb-3')
        )
    )
  ),
  shiny::tabPanel(title = 'Photo Submission',
    shiny::fluidRow(
      shiny::column(width = 4,
        card(title = shiny::tags$strong('Upload Photo'),
          dbPhotoUi('photo'),
          class = 'bg-secondary mb-3')
      ),
      shiny::column(width = 8,
        card(title = shiny::tags$strong('Photo'),
          dbShowPhotoUi('showPhoto'),
          class = 'bg-secondary mb-3')
      )
    )
  )
)

