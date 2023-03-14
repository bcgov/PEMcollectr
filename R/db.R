#' Create connection to postgres for shiny app
#'
#' @param dbname
#'
#' database name
#'
#' @param host
#'
#' server/host of postgres database
#'
#' @param port
#'
#' port number
#'
#' @param user
#'
#' user name
#'
#' @param password
#'
#' password
#'
#' @return
#'
#' DBI::dbConnect object for local development and pool::dbPool object for
#' deployment
#'
#' @export
#'
connect_pg <- function(dbname, host = Sys.getenv('PGHOST'), port = 5432,
  user = Sys.getenv('PGUSER'), password = Sys.getenv('PGPASSWORD')) {
  if (host == 'localhost') {
    DBI::dbConnect(drv = RPostgres::Postgres(), dbname = dbname, host = host,
      port = port, user = user, password = password)
  } else {
    pool::dbPool(drv = RPostgres::Postgres(), dbname = dbname, host = host,
      port = port, user = user, password = password)
  }
}
#' Append sf geometry table to a database table
#'
#' Checks column names from database and only selects those from data.frame
#'
#' @param con
#'
#' database connection object from connect_pg
#'
#' @param x
#'
#' sf object to write
#'
#' @param tableName
#'
#' name of table; can use DBI::SQL use non-default schema
#'
#' @export
#'
append_db <- function(con, x, tableName) {
  colNames <- names(x =
    DBI::dbGetQuery(conn = con, DBI::sqlInterpolate(conn = con,
      'SELECT * from ?tableName LIMIT 0', tableName = tableName)))
  x <- x[, colNames[!colNames %in% 'id']]
  sf::st_write(obj = x, dsn = con, tableName, append = TRUE)
}
#' Database upload module
#'
#' @param id
#'
#' namespace id
#'
#' @param sfObject
#'
#' shiny reactive sf object
#'
#' @param tableName
#'
#' name of table; can use DBI::SQL use non-default schema
#'
#' @param con
#'
#' database connection object from connect_pg
#'
#' @name dbWrite
#'
#' @export
#'
dbWriteUi <- function(id) {
  ns <- shiny::NS(id)
  shiny::tags$div(
    shiny::uiOutput(ns('submitIds')),
    shiny::uiOutput(ns('submitUi'))
  )
}
#' @rdname dbWrite
#'
#' @export
#'
dbWriteServer <- function(id, sfObject, tableName, con) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      success <- make_reactive_trigger()
      output$submitIds <- shiny::renderUI({
        shiny::req(sfObject())
        if (nrow(sfObject()) < 1) {
          shiny::tags$em('Select rows in Validation Results to submit')
        } else {
          shiny::tags$div(
            shiny::tags$h6('Upload datasets: '),
            shiny::tags$ul(lapply(unique(sfObject()[['transect_id']]),
              shiny::tags$li))
          )
        }
      })
      output$submitUi <- shiny::renderUI({
        shiny::req(nrow(sfObject()) > 0)
        shiny::actionButton(inputId = ns('submit'), label = 'Submit',
          class = 'btn-primary btn-sm')
      })
      shiny::observeEvent(input$submit, {
        shiny::req(nrow(sfObject()) > 0)
        sfObject <- sfObject()
        geometryType <- guess_geometry_type(sfObject = sfObject)
        if (geometryType == 'POINT') {
          # rename incidental transect_id
          sfObject[['transect_id']] <- add_incidental_ids(
            transectIds = sfObject[['transect_id']],
            dataType = sfObject[['data_type']],
            geometry = sfObject[['geom']])
          # if mapunit is not valid, then it needs review
          needsReview <- !vapply(lapply(sfObject[['mapunit1']],
            validate_membership(members = c(map_unit_veg(), map_unit_non_veg()))
          ), FUN = getElement, FUN.VALUE = logical(1), name = 'status')
          sfObject[['review']] <- needsReview
        }
        trySubmit <- tryCatch(append_db(con = con, x = sfObject,
          tableName = tableName()),
          error = identity)
        if (inherits(x = trySubmit, 'error')) {
          shiny::showNotification(ui = trySubmit$message, type = 'error')
        } else {
          shiny::showNotification(ui = 'Data uploaded', type = 'default')
          success$trigger()
        }
      })
      return(success)
    })
}
#' Check if transect is already in the database
#'
#' @param con
#'
#' connection to postgres
#'
#' @param transectIds
#'
#' vector of transect ids
#'
#' @param observers
#'
#' vector of observers
#'
#' @param stagingTable
#'
#' name of staging table \link{staging_tables}
#'
#' @param transectsTable
#'
#' name of transects table \link{transects_tables}
#'
#' @return
#'
#' boolean vector
#'
#' @export
is_in_db <- function(con, transectIds, observers, stagingTable,
  transectsTable) {
  if (stagingTable == staging_tables()[['POINT']]) {
    inDb <- DBI::dbGetQuery(con,
      sprintf('SELECT DISTINCT t.transect_id, observer
        FROM (SELECT transect_id, observer FROM %s UNION
        select transect_id, observer FROM %s) as t',
        stagingTable, transectsTable)
    )
    paste(transectIds, observers, sep = '-') %in%
      paste(inDb$transect_id, inDb$observer, sep = '-')
  } else {
    inDb <- DBI::dbGetQuery(con,
      sprintf('SELECT DISTINCT t.transect_id
        FROM (SELECT transect_id FROM %s UNION
        select transect_id FROM %s) as t',
        stagingTable, transectsTable)
    )
    transectIds %in% inDb$transect_id
  }
}
select_distinct_transect_ids <- function(con, stagingTable, transectsTable) {
  DBI::dbGetQuery(con,
    sprintf("SELECT DISTINCT t.transect_id
      FROM (SELECT transect_id, data_type FROM %s
      UNION
      select transect_id, data_type FROM %s ) as t
      where t.data_type <> 'incidental'", stagingTable,
      transectsTable)
  )[['transect_id']]
}
add_incidental_ids <- function(transectIds, dataType, geometry) {
  if (guess_geometry_type(geometry) == 'POINT') {
    ifelse(dataType == data_type()['incidental sampling'],
      apply(X = sf::st_coordinates(geometry), MARGIN = 1,
        FUN = paste, collapse = ',')
      , transectIds)
  } else {
    transectIds
  }
}
#' PEM database operations
#'
#' Runs a transaction to move data from staging to main table and
#' deletes the staging record if missing
#'
#' @param con
#'
#' connection to PEM database
#'
#' @param transectIds
#'
#' transect ids
#'
#' @export
#'
#' @name dbOperations
transfer_field_data_points <- function(con, transectIds) {
  msg <- tryCatch({
    DBI::dbBegin(conn = con)
    DBI::dbExecute(con,
      'CREATE TEMPORARY TABLE validatedFieldPoints (
      transect_id VARCHAR(255)
      )
      ON COMMIT DROP')
    sql <- DBI::sqlAppendTable(con = con,
      table = DBI::SQL('validatedFieldPoints'), values =
        data.frame(transect_id = transectIds), row.names = FALSE)
    DBI::dbExecute(con, sql)
    DBI::dbExecute(con,
      'INSERT INTO transects.field_data_points
      SELECT fdp.* FROM staging.field_data_points fdp
      JOIN validatedFieldPoints as vi on fdp.transect_id = vi.transect_id')
    DBI::dbExecute(con,
      'DELETE FROM staging.field_data_points as s
      USING transects.field_data_points t
      WHERE s.id = t.id')
    DBI::dbCommit(con)
  }, error = function(e) {
    DBI::dbRollback(con)
    e
    })
  msg
}
#' @param stagedIds
#'
#' integer id of staged tracklogs
#'
#' @export
#'
#' @name dbOperations
transfer_field_tracklog <- function(con, stagedIds) {
  msg <- tryCatch({
    DBI::dbBegin(conn = con)
    DBI::dbExecute(con,
      'CREATE TEMPORARY TABLE validatedTracklog (
      id INT
      )
      ON COMMIT DROP')
    sql <- DBI::sqlAppendTable(con = con,
      table = DBI::SQL('validatedTracklog'), values =
        data.frame(id = stagedIds), row.names = FALSE)
    DBI::dbExecute(con, sql)
    DBI::dbExecute(con,
      'INSERT INTO transects.field_tracklog
      SELECT ft.* FROM staging.field_tracklog ft
      JOIN validatedTracklog as vi on ft.id = vi.id')
    DBI::dbExecute(con,
      'DELETE FROM staging.field_tracklog as s
      USING transects.field_tracklog t
      WHERE s.id = t.id')
    DBI::dbCommit(con)
  }, error = function(e) {
    DBI::dbRollback(con)
    e
  })
  msg
}
#' @export
#'
#' @rdname dbPhoto
dbPhotoUi <- function(id) {
  ns <- shiny::NS(id)
  shiny::tags$div(
    shiny::uiOutput(ns('selectTransect')),
    shiny::uiOutput(ns('selectObserver')),
    shiny::uiOutput(ns('selectOrder')),
    shiny::fileInput(ns('photoFile'), label = 'Select Photo', width = '100%',
      accept = c('jpg', 'png', 'jpeg')),
    shiny::uiOutput(ns('submitUi')),
    leaflet::leafletOutput(ns('photoMap'))
  )
}
#' Module to select a point and upload a photo for it
#'
#' @param id
#'
#' namespace id
#'
#' @param con
#'
#' connection to postgres database
#'
#' @return
#'
#' a shiny reactive object row id of photo and a database trigger
#'
#' @export
#'
#' @name dbPhoto
#'
dbPhotoServer <- function(id, con) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      dbTrigger <- make_reactive_trigger()
      transectObserver <- shiny::reactive({
        DBI::dbGetQuery(conn = con,
          sprintf('SELECT DISTINCT t.transect_id, observer, t.order
        FROM (SELECT transect_id, observer, "order" FROM %s UNION
        select transect_id, observer, "order" FROM %s) as t',
            staging_tables()[['POINT']],
            transects_tables()[['POINT']]))
      })
      output$selectTransect <- shiny::renderUI({
        shiny::selectizeInput(ns('selectedTransect'), 'Select Transect ID',
          choices = sort(unique(transectObserver()[['transect_id']])),
          width = '100%')
      })
      output$selectObserver <- shiny::renderUI({
        observers <- transectObserver()[['observer']][
          transectObserver()[['transect_id']] == input$selectedTransect]
        shiny::selectizeInput(ns('selectedObserver'), 'Select Observer',
          choices = sort(unique(observers)),
          width = '100%')
      })
      output$selectOrder <- shiny::renderUI({
        orders <- transectObserver()[['order']][
          transectObserver()[['transect_id']] == input$selectedTransect &
            transectObserver()[['observer']] == input$selectedObserver]
        shiny::selectizeInput(ns('selectedOrder'), 'Select Order',
          choices = sort(unique(orders)),
          width = '100%')
      })
      output$submitUi <- shiny::renderUI({
        shiny::req(input$selectedTransect, input$selectedObserver,
          input$selectedOrder, input$photoFile)
        shiny::actionButton(inputId = ns('submit'), 'Submit',
          class = 'btn-primary btn-sm mb-3')
      })
      mapData <- shiny::reactive({
        shiny::req(input$selectedTransect, input$selectedObserver)
        query <- sprintf("SELECT t.*, p.id is not null as has_photo
        FROM (SELECT * FROM %s UNION
        select * FROM %s) as t
          left join transects.photos as p on t.id = p.id
            where t.transect_id = \'%s\' and t.observer = \'%s\'",
          staging_tables()[['POINT']],
          transects_tables()[['POINT']],
          input$selectedTransect, input$selectedObserver)
        sf::st_transform(sf::st_read(dsn = con, query = query), crs = 4326)
      })
      output$photoMap <- leaflet::renderLeaflet({
        shiny::req(mapData(), input$selectedOrder)
        bb <- sf::st_bbox(mapData())
        leaflet::leaflet() |>
          leaflet::addProviderTiles(
            provider = leaflet::providers$Esri.WorldTopoMap) |>
          leaflet::fitBounds(lng1 = bb[['xmin']], lat1 = bb[['ymin']],
            lng2 = bb[['xmax']], lat2 = bb[['ymax']])

      })
      shiny::observeEvent(input$photoMap_marker_click, {
        clickId <- stats::setNames(
          strsplit(input$photoMap_marker_click$id, split = '-')[[1]],
          c('transect_id', 'observer', 'order'))
        shiny::updateSelectizeInput(session = session,
          inputId = 'selectedOrder', selected = clickId[['order']])
      })
      shiny::observeEvent(input$selectedOrder, {
        colors <- ifelse(mapData()[['has_photo']], zissou()[6],
          zissou()[1])
        colors[mapData()[['order']] == input$selectedOrder] <- zissou()[7]
        leaflet::leafletProxy('photoMap') |>
          leaflet::addCircleMarkers(
            data = mapData(),
            radius = 5, color = colors,
            opacity = .5, fillOpacity = .5, weight = 1,
            group = 'photoPoints',
            layerId = ~sprintf('%s-%s-%s', transect_id, observer, order),
            popup = ~sprintf('%s-%s-%s', transect_id, observer, order))
      })
      photoId <- shiny::reactive({
        shiny::req(input$selectedTransect, input$selectedObserver,
          input$selectedOrder)
        get_point_id(con = con, transectId = input$selectedTransect,
          observer = input$selectedObserver,
          order = input$selectedOrder)
      })
      shiny::observeEvent(input$submit, {
        shiny::req(input$selectedTransect, input$selectedObserver,
          input$selectedOrder, input$photoFile)
        id <- photoId()
        photo <- blob::as_blob(readBin(input$photoFile$datapath, what = 'raw',
          n = 1000000))
        ext <- sub('.+\\.([A-Za-z]+)', '\\1', input$photoFile$datapath)
        binPhoto <- data.table::data.table(id, photo, ext)
        writeTransaction <- upsert_photo(con, binPhoto)
        if (inherits(writeTransaction, 'error')) {
          shiny::showNotification(ui = writeTransaction$message, type = 'error')
        } else {
          shiny::showNotification(ui = 'Photo uploaded.',
            type = 'default')
          dbTrigger$trigger()
        }
      })

      selectedPoint <- shiny::reactive({
        list(id = photoId(), success = dbTrigger$depend()
        )
      })
      return(selectedPoint)

    })
}
#' Add/Update photo to photo table
#'
#' @param con
#'
#' connection to postgres
#'
#' @param photoDf
#'
#' dataframe with photo as a blob
#'
#' @return
#'
#' error or TRUE if successful
#'
#' @export
upsert_photo <- function(con, photoDf) {
  msg <- tryCatch({
    DBI::dbBegin(conn = con)
    DBI::dbExecute(con,
      sprintf('DELETE FROM transects.photos where id = %d', photoDf[['id']]))
    DBI::dbExecute(conn = con,
      DBI::sqlAppendTable(con = con, table = DBI::SQL('transects.photos'),
        values = photoDf, row.names = FALSE))
    DBI::dbCommit(con)
  }, error = function(e) {
    DBI::dbRollback(con)
    e
  })
  msg
}
#' Get the unique data point id from staging/main table
#'
#' @param con
#'
#' connection to postgres
#'
#' @param transectId
#'
#' transect id
#'
#' @param observer
#'
#' observer
#'
#' @param order
#'
#' point order of sampling
#'
#'
#' @return
#'
#' integer
#'
#' @export
#'
get_point_id <- function(con, transectId, observer, order) {
  DBI::dbGetQuery(conn = con,
    sprintf('SELECT id
        FROM (SELECT "id", transect_id, observer, "order" FROM %s UNION
        select "id", transect_id, observer, "order" FROM %s) as t
            where transect_id in (\'%s\') and observer in (\'%s\')
            and "order" in (%s)',
      staging_tables()[['POINT']],
      transects_tables()[['POINT']],
      transectId, observer, order))[['id']]
}
#' @export
#'
#' @name ShowPhoto
dbShowPhotoUi <- function(id) {
  ns <- shiny::NS(id)
  shiny::tags$div(shiny::imageOutput(ns('photoUi')),
    style = 'height: 700px;')
}
#' Queries a photo from the database and shows the image
#'
#' @param id
#'
#' namespace id
#'
#' @param con
#'
#' connection to postgres
#'
#' @param selectedPoint
#'
#' reactive list with id of point in database and a database trigger
#'
#' @export
#'
#' @name ShowPhoto
#'
dbShowPhotoServer <- function(id, con, selectedPoint) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      output$photoUi <- shiny::renderImage({
        shiny::req(selectedPoint()[['id']])
        outfile <- tryCatch({
          convert_to_image(con, id = selectedPoint()[['id']])
        }, error = identity)
        shiny::req(!inherits(outfile, 'error'))
        list(src = outfile,
          contentType = 'image/jpg',
          style = 'display: flex; margin: auto;',
          height = 700,
          alt = "This is alternate text")
      }, deleteFile = TRUE)
    })
}
#' Converts binary data of photo in database to an on-disk image
#'
#' @param con
#'
#' connection to postgres
#'
#' @param id
#'
#' unique data point id
#'
#' @return
#'
#' name of temp file where image is stored
#'
#' @export
#'
convert_to_image <- function(con, id) {
  photo <- DBI::dbGetQuery(con,
    sprintf('select * from transects.photos where "id" in (%d)', id))
  temp <- tempfile(fileext = sprintf('.%s', photo[['ext']]))
  writeBin(photo[['photo']][[1]], con = temp, useBytes = TRUE)
  temp
}
