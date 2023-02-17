shiny::fluidPage(
  shiny::fluidRow(
    shiny::column(width = 3,
      geomUploadUi('geomUpload'),
      shiny::tags$div(style = 'height: 20px;'),
      dbWriteUi('dbWrite')
    ),
    shiny::column(width = 9,
      validateTableUi('validateTable')
    )
  )
)
