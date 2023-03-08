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
  x <- x[ , colNames[!colNames %in% 'id']]
  sf::st_write(obj = x, dsn = con, tableName, append = TRUE)
}

#' Databse upload module
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
