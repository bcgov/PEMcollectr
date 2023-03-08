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
      multiple = TRUE, width = '100%',
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
            value = TRUE), width = '100%')
      })
      output$selectLayerUi <- shiny::renderUI({
        shiny::req(filePath())
        layerNames <- sf::st_layers(filePath())
        shiny::selectInput(inputId = ns('selectedLayer'),
          label = 'Select Layer', choices = layerNames$name, width = '100%')
      })
      filePath <- shiny::reactive({
        shiny::req(input$selectedFile)
        if (isFALSE(check_req_files(selectedFile = input$selectedFile,
          uploadedFiles = uploadedFiles()[['name']]))) {
          shiny::showNotification('Missing required files for uploading data.
            Shapefiles require cpg, dbf, prj, shp, shx', duration = NULL,
            type = 'error')
          return(NULL)
        }
        uploadedFiles()[['datapath']][
          uploadedFiles()[['name']] == input$selectedFile]
      })
      output$validateUi <- shiny::renderUI({
        shiny::req(input$selectedLayer)
        shiny::actionButton(inputId = ns('validateFile'), 'Validate',
          class = 'btn-primary btn-sm')
      })
      sfObject <- shiny::eventReactive(input$validateFile, {
        sfObject <- sf::st_read(dsn = filePath(), layer = input$selectedLayer,
          quiet = TRUE)
        containsTransectId <- 'transect_id' %in% names(sfObject)
        containsObserver <- 'observer' %in% names(sfObject)
        geometryType <- guess_geometry_type(sfObject)
        if (geometryType %in% 'POINT') {
          if (!containsTransectId ||
              !'observer' %in% names(sfObject)) {
            shiny::showNotification(ui =
                'Data must contain at minimum a "transect_id" and "observer" column.',
              duration = NULL, type = 'error')
            return(NULL)
          }
        } else if (geometryType %in% c('LINESTRING', 'MULTILINESTRING')) {
          if (!containsTransectId) {
            shiny::showNotification(ui =
                'Data must contain at minimum a "transect_id" column.',
              duration = NULL, type = 'error')
            browser()
            return(NULL)
          }
        } else {
          shiny::showNotification(ui =
              'Data must contain point or line geometry.',
            duration = NULL, type = 'error')
          return(NULL)
        }
        sfObject[['transect_id']][
          sfObject[['data_type']] == data_type()['incidental sampling']] <-
          'incidental'
        sfObject
      })
      return(sfObject)
    }
  )
}
check_req_files <- function(selectedFile, uploadedFiles) {
  shpExt <- '\\.shp$'
  if (grepl(shpExt, selectedFile)) {
    all(sprintf('%s.%s', sub(shpExt, '', selectedFile),
      c('cpg', 'dbf', 'prj', 'shp', 'shx')) %in% uploadedFiles)
  } else {
    TRUE
  }
}
#' Guess the geometry type of  a sf or sfc object
#'
#' @param sfObject
#'
#' sf dataframe object
#'
#' @return
#'
#' character vector size 1 with geometry type
#'
#' @export
#'
guess_geometry_type <- function(sfObject) {
  as.character(unique(sf::st_geometry_type(sfObject))[1])
}
