#' Shiny Module to upload geometry
#'
#' @param id
#'
#' namespace id
#'
#' @return
#'
#' reactive value of sf object
#'
#' @export
#'
#' @name geomUpload
geomUploadUi <- function(id) {
  ns <- shiny::NS(id)
  shiny::tags$div(
    shiny::fileInput(ns('geomFile'), label = 'Upload Geometry Files',
      multiple = TRUE,
      #TODO: add kml
      accept = c('.gpkg', '.shp', '.cpg', '.dbf', '.prj', '.shx'),
      buttonLabel = 'Browse')
    , shiny::uiOutput(ns('selectFileUi'))
    , shiny::uiOutput(ns('selectLayerUi'))
    , shiny::uiOutput(ns('validateUi'))
  )
}
#' @export
#'
#' @rdname geomUpload
geomUploadServer <- function(id) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      uploadedFiles <- shiny::reactiveVal()
      # rename files because reading shapefiles requires multiple files
      # with the same names and different extensions
      shiny::observeEvent(input$geomFile, {
        stopifnot(!anyDuplicated(input$geomFile$name))
        newFileNames <- paste(
          sub('[0-9]+\\.[A-Za-z]+', '', input$geomFile$datapath),
          input$geomFile$name, sep = '')
        file.rename(input$geomFile$datapath, newFileNames)
        uploadedFiles <- input$geomFile
        uploadedFiles[['datapath']] <- newFileNames
        uploadedFiles(uploadedFiles)
      })
      output$selectFileUi <- shiny::renderUI({
        shiny::req(uploadedFiles())
        shiny::selectInput(inputId = ns('selectedFile'), label = 'Select File',
          #TODO: add kml
          choices = grep('\\.(gpkg|shp)$', uploadedFiles()[['name']],
            value = TRUE))
      })
      output$selectLayerUi <- shiny::renderUI({
        shiny::req(filePath())
        layerNames <- sf::st_layers(filePath())
        shiny::selectInput(inputId = ns('selectedLayer'),
          label = 'Select Layer', choices = layerNames$name)
      })
      filePath <- shiny::reactive({
        shiny::req(input$selectedFile)
        uploadedFiles()[['datapath']][
          uploadedFiles()[['name']] == input$selectedFile]
      })
      output$validateUi <- shiny::renderUI({
        shiny::req(input$selectedLayer)
        shiny::actionButton(inputId = ns('validateFile'), 'Validate')
      })
      sfObject <- shiny::eventReactive(input$validateFile, {
        sf::st_read(dsn = filePath(), layer = input$selectedLayer,
          quiet = TRUE)
      })
      return(sfObject)
    }
  )
}
