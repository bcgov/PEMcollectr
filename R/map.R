#' Shiny module to show PEM data
#'
#' @param id
#'
#' namespace id
#'
#' @param con
#'
#' connection to postgres database
#'
#' @export
#'
#' @name PEMmapUi
mapUi <- function(id) {
  ns <- shiny::NS(id)
  shiny::tags$div(leaflet::leafletOutput(ns('baseMap'), height = 750))
}
#' @export
#'
#' @rdname PEMmapUi
mapServer <- function(id, con) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      init <- shiny::reactiveVal()
      pointSymbol <- leaflegend::makeSymbolIcons(shape = 'circle', width = 10,
        color = zissou()[1], opacity = .5)
      samplePlan <- shiny::reactive({
        tryCatch(
          sf::st_transform(
            sf::st_read(dsn = con, layer = DBI::SQL('transects.sample_plan')),
            crs = 4326),
          error = identity)
      })
      pointData <- shiny::reactive({
        tryCatch(
          sf::st_transform(
            sf::st_read(dsn = con,
              layer = DBI::SQL('transects.field_data_points')),
            crs = 4326),
          error = identity)
      })
      tracklog <- shiny::reactive({
        tryCatch(
          sf::st_zm(sf::st_transform(
            sf::st_read(dsn = con,
              layer = DBI::SQL('transects.field_tracklog')),
            crs = 4326)),
          error = identity)
      })
      output$baseMap <- leaflet::renderLeaflet({
        bb <- sf::st_bbox(samplePlan())
        map <- leaflet::leaflet() |>
          leaflet::addProviderTiles(
            provider = leaflet::providers$Esri.WorldTopoMap) |>
          leaflet::fitBounds(lng1 = bb[['xmin']], lat1 =bb[['ymin']],
            lng2 = bb[['xmax']], lat2 = bb[['ymax']]) |>
          leaflet::addControl(card(title = shiny::tags$strong('Map Layers',
            style='font-size: 16px;'),
            switchInput(ns('toggleSamplePlan'), shiny::tags$div(
              shiny::tags$img(src =
                  leaflegend::makeSymbol('triangle', color = 'black',
                    width = 20, opacity = 1, fillColor = 'transparent')),
              'Sample Plan'), value = TRUE),
            switchInput(ns('togglePoints'), shiny::tags$div(
              shiny::tags$img(src =  leaflegend::makeSymbol(
                shape = 'circle', width = 20, color = zissou()[1],
                opacity = .5)),
              'Collected Points'), value = TRUE),
            switchInput(ns('toggleTracklog'), shiny::tags$div(
              shiny::tags$img(src = leaflegend::makeSymbol('rect', width = 20,
                height= 5, color = zissou()[6], fillColor = zissou()[6])),
                'Collected Tracklog'),
              value = TRUE),
            switchInput(ns('toggleComplete'), shiny::tags$div(
              shiny::tags$img(src =
                  leaflegend::makeSymbol('triangle', color = zissou()[7],
                    width = 20, opacity = 1, fillColor = 'transparent')),
                'Completed'), value = FALSE),
            class = 'bg-secondary mb-3', style='width: 300px'), className = '',
            position = 'topright')
        init(TRUE)
        map
      })
      shiny::observeEvent(input$toggleSamplePlan, {
        toggle <- input$toggleSamplePlan
        shiny::req(!is.null(toggle))
        if (isTRUE(toggle)) {
          leaflet::leafletProxy(mapId = 'baseMap', data = samplePlan()) |>
            leaflet::addPolylines(color='black', weight = 2, opacity = 1,
              group = 'samplePlan', layerId = ~transect_id)
        } else {
          leaflet::leafletProxy(mapId = 'baseMap') |>
            leaflet::clearGroup(group = 'samplePlan')
        }
      })
      shiny::observeEvent(input$togglePoints, {
        toggle <- input$togglePoints
        shiny::req(!is.null(toggle))
        if (isTRUE(toggle)) {
          leaflet::leafletProxy(mapId = 'baseMap', data = pointData()) |>
            leaflet::addMarkers(icon = pointSymbol,
              group = 'points')
        } else {
          leaflet::leafletProxy(mapId = 'baseMap') |>
            leaflet::clearGroup(group = 'points')
        }
      })
      shiny::observeEvent(input$toggleTracklog, {
        toggle <- input$toggleTracklog
        shiny::req(!is.null(toggle))
        if (isTRUE(toggle)) {
          leaflet::leafletProxy(mapId = 'baseMap', data = tracklog()) |>
            leaflet::addPolylines(color= zissou()[6], weight = 2, opacity = 1,
              group = 'tracklog')
        } else {
          leaflet::leafletProxy(mapId = 'baseMap') |>
            leaflet::clearGroup(group = 'tracklog')
        }
      })

    })
}
