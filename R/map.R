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
  shiny::tags$div(leaflet::leafletOutput(ns('baseMap'), height = 600))
}
#' @param sfObject
#'
#' uploaded sf object
#'
#' @param success
#'
#' shiny db trigger
#'
#' @export
#'
#' @rdname PEMmapUi
mapServer <- function(id, con, sfObject, success) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      redPoint <- leaflegend::makeSymbol(shape = 'circle',
        width = 10, color = zissou()[7], opacity = .5)
      orangePoint <- leaflegend::makeSymbol(shape = 'circle',
        width = 10, color = zissou()[6], opacity = .5)
      bluePoint <- leaflegend::makeSymbol(shape = 'circle',
        width = 10, color = zissou()[1], opacity = .5)
      blackTriangle <- leaflegend::makeSymbol('triangle', color = 'black',
        width = 20, opacity = 1, fillColor = 'transparent')
      orangeTriangle <- leaflegend::makeSymbol('triangle', color = zissou()[6],
        width = 20, opacity = 1, fillColor = 'transparent')
      blueTriangle <- leaflegend::makeSymbol('triangle', color = zissou()[1],
        width = 20, opacity = 1, fillColor = 'transparent')
      redLine <- leaflegend::makeSymbol('rect', width = 20,
        height = 5, color = zissou()[7], fillColor = zissou()[7])
      orangeLine <- leaflegend::makeSymbol('rect', width = 20,
        height = 5, color = zissou()[6], fillColor = zissou()[6])
      blueLine <- leaflegend::makeSymbol('rect', width = 20,
        height = 5, color = zissou()[1], fillColor = zissou()[1])
      aoi <- shiny::reactive({
        tryCatch(
          sf::st_transform(
            sf::st_read(dsn = con,
              layer = DBI::SQL('transects.aoi')),
            crs = 4326),
          error = identity)
      })
      output$baseMap <- leaflet::renderLeaflet({
        bb <- sf::st_bbox(samplePlan())
        map <- leaflet::leaflet(
          options = leaflet::leafletOptions(preferCanvas = TRUE)) |>
          leaflet::addProviderTiles(
            provider = leaflet::providers$Esri.WorldTopoMap,
            options = leaflet::providerTileOptions(
              updateWhenZooming = FALSE)) |>
          leaflet::fitBounds(lng1 = bb[['xmin']], lat1 = bb[['ymin']],
            lng2 = bb[['xmax']], lat2 = bb[['ymax']]) |>
          leaflet::addControl(card(title = shiny::tags$strong('Map Layers',
            style = 'font-size: 16px;'),
            shiny::tags$h6('Sample Plan', class = 'mt-0 font-weight-bold'),
            shiny::tags$div(class = 'header-divider'),
            switchInput(ns('toggleSamplePlan'), shiny::tags$div(
              shiny::tags$img(src = blackTriangle),
              'All'), value = TRUE, class = 'ms-3'),
            switchInput(ns('togglePending'), shiny::tags$div(
              shiny::tags$img(src = orangeTriangle),
              'Pending'), value = FALSE, class = 'ms-3'),
            switchInput(ns('toggleComplete'), shiny::tags$div(
              shiny::tags$img(src = blueTriangle),
              'Completed'), value = FALSE, class = 'ms-3'),
            shiny::tags$h6('Points'),
            shiny::tags$div(class = 'header-divider'),
            switchInput(ns('toggleUploadedPoints'), shiny::tags$div(
              shiny::tags$img(src = redPoint),
              'Uploaded'), value = FALSE, class = 'ms-3'),
            switchInput(ns('toggleStagedPoints'), shiny::tags$div(
              shiny::tags$img(src =  orangePoint),
              'Staged'), value = FALSE, class = 'ms-3'),
            switchInput(ns('togglePoints'), shiny::tags$div(
              shiny::tags$img(src =  bluePoint),
              'Completed'), value = FALSE, class = 'ms-3'),
            shiny::tags$h6('Tracklog'),
            shiny::tags$div(class = 'header-divider'),
            switchInput(ns('toggleUploadedTracklog'), shiny::tags$div(
              shiny::tags$img(src = redLine), 'Uploaded'),
              value = FALSE, class = 'ms-3'),
            switchInput(ns('toggleStagedTracklog'), shiny::tags$div(
              shiny::tags$img(src = orangeLine), 'Staged'),
              value = FALSE, class = 'ms-3'),
            switchInput(ns('toggleTracklog'), shiny::tags$div(
              shiny::tags$img(src = blueLine), 'Completed'),
              value = FALSE, class = 'ms-3'),
            class = 'bg-secondary mb-3', style = 'width: 225px'),
            className = '', position = 'topright') |>
          leaflet::addPolylines(data = aoi(), popup = ~name,
            color = '#003366', opacity = 1, weight = 3) |>
          leaflet::addLabelOnlyMarkers(data = find_boundary_point(aoi()),
            options = leaflet::markerOptions(zIndexOffset = 10),
            label = ~name, labelOptions = leaflet::labelOptions(
              textsize = '12px', offset = c(0, -10),
              noHide = TRUE, textOnly = TRUE, direction = 'center',
              style = list('color' =  '#003366', 'font-weight' = 'bold')),
            layerId = ~name)
        map
      })
      shiny::observeEvent(input$toggleSamplePlan, {
        toggle <- input$toggleSamplePlan
        shiny::req(!is.null(toggle))
        if (isTRUE(toggle)) {
          leaflet::leafletProxy(mapId = 'baseMap', data = samplePlan()) |>
            leaflet::addPolylines(color = 'black', weight = 2, opacity = 1,
              group = 'samplePlan', layerId = ~transect_id)
          updateSwitchInput(session = session,
            inputId = 'toggleComplete', value = FALSE)
          updateSwitchInput(session = session,
            inputId = 'togglePending', value = FALSE)
        } else {
          leaflet::leafletProxy(mapId = 'baseMap') |>
            leaflet::clearGroup(group = 'samplePlan')
        }
      })
      # sample plan
      samplePlan <- shiny::reactive({
        tryCatch(
          sf::st_read(dsn = con, layer = DBI::SQL('transects.sample_plan')),
          error = identity)
      })
      completedSamplePlan <- shiny::reactive({
        transectIds <- sort(unique(pointData()[['transect_id']][
          pointData()[['data_type']] != data_type()['incidental sampling']]))
        validatePairs <- vapply(
          validate_sample_pairs(transectIds = transectIds), FUN = getElement,
          FUN.VALUE = character(1), name = 'messages')
        validatePairs <- validatePairs[validatePairs == '']
        validatePairs <- expand.grid(x = names(validatePairs),
          y = transect_location())
        validatePairs <- data.frame(transect_id = sprintf('%s_%s',
          validatePairs[['x']], validatePairs[['y']]))
        merge(samplePlan(), validatePairs, by = 'transect_id')
      })
      pendingSamplePlan <- shiny::reactive({
        transectIds <- sort(unique(stagedPointData()[['transect_id']][
          pointData()[['data_type']] != data_type()['incidental sampling']]))
        pattern <- sprintf('_(%s)$', paste(transect_location(), collapse = '|'))
        incomplete <- transectIds[!sub(pattern, '', transectIds) %in%
          unique(sub(pattern, '', completedSamplePlan()[['transect_id']]))]
        partials <- unique(pointData()[['transect_id']][
          sub(pattern, '', pointData()[['transect_id']])
          %in% sub(pattern, '', incomplete)])
        validatePairs <- vapply(
          validate_sample_pairs(transectIds = c(incomplete, partials)),
          FUN = getElement,
          FUN.VALUE = character(1), name = 'messages')
        validatePairs <- validatePairs[validatePairs == '']
        validatePairs <- expand.grid(x = names(validatePairs),
          y = transect_location())
        validatePairs <- data.frame(transect_id = sprintf('%s_%s',
          validatePairs[['x']], validatePairs[['y']]))
        merge(samplePlan(), validatePairs, by = 'transect_id')
      })
      shiny::observeEvent(input$togglePending, {
        toggle <- input$togglePending
        shiny::req(!is.null(toggle))
        if (isTRUE(toggle)) {
          leaflet::leafletProxy(mapId = 'baseMap',
            data = pendingSamplePlan()) |>
            leaflet::addPolylines(color = zissou()[6], weight = 2, opacity = 1,
              group = 'pending', layerId = ~transect_id)
        } else {
          if (input$toggleSamplePlan) {
            leaflet::leafletProxy(mapId = 'baseMap',
              data = pendingSamplePlan()) |>
              leaflet::addPolylines(color = 'black', weight = 2, opacity = 1,
                group = 'samplePlan', layerId = ~transect_id,
                popup = ~transect_id)
          } else {
            leaflet::leafletProxy(mapId = 'baseMap') |>
              leaflet::clearGroup(group = 'pending')
          }
        }
      })
      shiny::observeEvent(input$toggleComplete, {
        toggle <- input$toggleComplete
        shiny::req(!is.null(toggle), completedSamplePlan())
        if (isTRUE(toggle)) {
          leaflet::leafletProxy(mapId = 'baseMap',
            data = completedSamplePlan()) |>
            leaflet::addPolylines(color = zissou()[1], weight = 2, opacity = 1,
              group = 'completed', layerId = ~transect_id)
        } else {
          if (input$toggleSamplePlan) {
            leaflet::leafletProxy(mapId = 'baseMap',
              data = completedSamplePlan()) |>
              leaflet::addPolylines(color = 'black', weight = 2, opacity = 1,
                group = 'samplePlan', layerId = ~transect_id,
                popup = ~transect_id)
          } else {
            leaflet::leafletProxy(mapId = 'baseMap') |>
              leaflet::clearGroup(group = 'completed')
          }
        }
      })
      # uploaded data
      shiny::observeEvent(list(sfObject(), input$toggleUploadedPoints,
        input$toggleUploadedTracklog), {
        shiny::req(sfObject())
        geometryType <- guess_geometry_type(sfObject())
        transectIds <- add_incidental_ids(
          transectIds = sfObject()[['transect_id']],
          dataType = sfObject()[['data_type']],
          geometry = sf::st_geometry(sfObject()))
        sfObject <- sfObject()[!is_in_db(con = con,
          transectIds = transectIds,
          observers = sfObject()[['observer']],
          stagingTable = staging_tables()[geometryType],
          transectsTable = transects_tables()[geometryType]), ] |>
          sf::st_transform(crs = 4326)
        sfObject <- sfObject[sfObject[['data_type']] !=
            data_type()['incidental sampling'], ]
        if (geometryType %in% 'POINT') {
          if (isTRUE(input$toggleUploadedPoints)) {
            updateSwitchInput(session = session,
              inputId = 'toggleUploadedTracklog', value = FALSE)
            leaflet::leafletProxy(mapId = 'baseMap',
              data = sfObject) |>
              leaflet::clearGroup(group = 'uploadedTracklog') |>
              leaflet::addCircleMarkers(
                radius = 5, color = zissou()[7], fillColor = zissou()[7],
                opacity = .5, fillOpacity = .5, weight = 1,
                group = 'uploadedPoints',
                layerId = ~sprintf('%s-%s', transect_id, order),
                popup = ~sprintf('%s-%s', transect_id, order))
          } else {
            leaflet::leafletProxy(mapId = 'baseMap') |>
              leaflet::clearGroup(group = 'uploadedPoints')
          }
        } else if (geometryType %in% c('LINESTRING', 'MULTILINESTRING')) {
          if (isTRUE(input$toggleUploadedTracklog)) {
            updateSwitchInput(session = session,
              inputId = 'toggleUploadedPoints', value = FALSE)
            leaflet::leafletProxy(mapId = 'baseMap',
              data = sfObject) |>
              leaflet::clearGroup(group = 'uploadedPoints') |>
              leaflet::addPolylines(color = zissou()[7], weight = 2,
                opacity = 1,
                group = 'uploadedTracklog',
                layerId = ~sprintf('%s-tracklog', transect_id),
                popup = ~sprintf('%s-tracklog', transect_id))
          } else {
            leaflet::leafletProxy(mapId = 'baseMap') |>
              leaflet::clearGroup(group = 'uploadedTracklog')
          }
        } else {
          shiny::showNotification(
            ui =
              'Unsupported geometry type or mixed geometry for uploaded data',
            duration = NULL, type = 'error')
          return(NULL)
        }
      })
      # staged data
      stagedPointData <- shiny::reactive({
        success$depend()
        tryCatch(
          sf::st_transform(
            sf::st_read(dsn = con,
              layer = DBI::SQL('staging.field_data_points')),
            crs = 4326),
          error = identity)
      })
      shiny::observeEvent(input$toggleStagedPoints, {
        toggle <- input$toggleStagedPoints
        shiny::req(!is.null(toggle), !inherits(stagedPointData(), 'error'))
        if (isTRUE(toggle)) {
          leaflet::leafletProxy(mapId = 'baseMap',
            data = stagedPointData()[stagedPointData()[['data_type']] !=
              data_type()['incidental sampling'], ]) |>
            leaflet::addCircleMarkers(
              radius = 5, color = zissou()[6], fillColor = zissou()[6],
              opacity = .5, fillOpacity = .5, weight = 1,
              group = 'stagedPoints',
              layerId = ~sprintf('%s-%s', transect_id, order),
              popup = ~sprintf('%s-%s', transect_id, order))
        } else {
          leaflet::leafletProxy(mapId = 'baseMap') |>
            leaflet::clearGroup(group = 'stagedPoints')
        }
      })
      stagedTracklog <- shiny::reactive({
        success$depend()
        tryCatch(
          sf::st_zm(sf::st_transform(
            sf::st_read(dsn = con,
              layer = DBI::SQL('staging.field_tracklog')),
            crs = 4326)),
          error = identity)
      })
      shiny::observeEvent(input$toggleStagedTracklog, {
        toggle <- input$toggleStagedTracklog
        shiny::req(!is.null(toggle), !inherits(stagedTracklog(), 'error'))
        if (isTRUE(toggle)) {
          leaflet::leafletProxy(mapId = 'baseMap', data = stagedTracklog()) |>
            leaflet::addPolylines(color = zissou()[6], weight = 2, opacity = 1,
              group = 'stagedTracklog',
              layerId = ~sprintf('%s-tracklog', transect_id),
              popup = ~sprintf('%s-tracklog', transect_id))
        } else {
          leaflet::leafletProxy(mapId = 'baseMap') |>
            leaflet::clearGroup(group = 'stagedTracklog')
        }
      })
      # completed data
      pointData <- shiny::reactive({
        tryCatch(
          sf::st_transform(
            sf::st_read(dsn = con,
              layer = DBI::SQL('transects.field_data_points')),
            crs = 4326),
          error = identity)
      })
      shiny::observeEvent(input$togglePoints, {
        toggle <- input$togglePoints
        shiny::req(!is.null(toggle), !inherits(pointData(), 'error'))
        if (isTRUE(toggle)) {
          leaflet::leafletProxy(mapId = 'baseMap',
            data = pointData()[pointData()[['data_type']] !=
                data_type()['incidental sampling'], ]) |>
            leaflet::addCircleMarkers(
              radius = 5, color = zissou()[1], fillColor = zissou()[1],
              opacity = .5, fillOpacity = .5, weight = 1,
              group = 'completedPoints',
              layerId = ~sprintf('%s-%s', transect_id, order),
              popup = ~sprintf('%s-%s', transect_id, order))
        } else {
          leaflet::leafletProxy(mapId = 'baseMap') |>
            leaflet::clearGroup(group = 'completedPoints')
        }
      })
      tracklog <- shiny::reactive({
        tryCatch(
          sf::st_zm(sf::st_transform(
            sf::st_read(dsn = con,
              layer = DBI::SQL('transects.field_tracklog')),
            crs = 4326)),
          error = identity)
      })
      shiny::observeEvent(input$toggleTracklog, {
        toggle <- input$toggleTracklog
        shiny::req(!is.null(toggle), !inherits(tracklog(), 'error'))
        if (isTRUE(toggle)) {
          leaflet::leafletProxy(mapId = 'baseMap', data = tracklog()) |>
            leaflet::addPolylines(color = zissou()[1], weight = 2, opacity = 1,
              group = 'tracklog',
              layerId = ~sprintf('%s-tracklog', transect_id),
              popup = ~sprintf('%s-tracklog', transect_id))
        } else {
          leaflet::leafletProxy(mapId = 'baseMap') |>
            leaflet::clearGroup(group = 'tracklog')
        }
      })

    })
}
#' Find boundary point of geometry
#'
#' @param sfObject
#'
#' sf data.frame
#'
#' @param coord
#'
#' which axis to find extreme point
#'
#' @param type
#'
#' which direction min/max
#'
#' @return
#'
#' sf dataframe
#'
#' @export
#'
find_boundary_point <- function(sfObject, coord = c('lat', 'lng'),
  type = c('min', 'max')) {
  coord <- match.arg(coord)
  coord <- ifelse(coord == 'lat', 2, 1)
  type <- match.arg(type)
  decreasing <- type == 'max'
  geometry <- lapply(sf::st_geometry(sfObject), as.matrix)
  minCoord <- lapply(geometry, function(x, col) {
    sf::st_point(x[order(x[, coord], decreasing = decreasing), ][1, ])
  }, col = coord)
  sf::st_as_sf(
    cbind(sf::st_drop_geometry(sfObject), geom = sf::st_as_sfc(minCoord)),
    crs = sf::st_crs(sfObject)
  )
}
