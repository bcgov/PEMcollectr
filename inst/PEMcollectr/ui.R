library(shiny)
shiny::fluidPage(
  shiny::fluidRow(
    shiny::column(width = 3,
      geomUploadUi('geomUpload')
    ),
    shiny::column(width = 6,
      shiny::tableOutput('dataValidation')
    )
  )
)
