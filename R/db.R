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

#' Title
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
      output$submitIds <- shiny::renderUI({
        shiny::req(nrow(sfObject()) > 0)
        shiny::tags$div(
          shiny::tags$h6('Upload datasets: '),
          shiny::tags$ul(lapply(unique(sfObject()[['transect_id']]),
            shiny::tags$li))
        )
      })
      output$submitUi <- shiny::renderUI({
        shiny::req(nrow(sfObject()) > 0)
        shiny::actionButton(inputId = ns('submit'), label = 'Submit')
      })
      shiny::observeEvent(input$submit, {
        shiny::req(nrow(sfObject()) > 0)
        trySubmit <- tryCatch(append_db(con = con, x = sfObject,
          tableName = tableName),
          error = identity)
        if (inherits(x = trySubmit, 'error')) {
          shiny::showNotification(ui = trySubmit$message, type = 'error')
        }
      })
    })
}

check_in_db <- function(con, x, tableName) {
  inDb <- DBI::dbGetQuery(con,
    sprintf('SELECT transect_id FROM %s WHERE transect_id in (%s)',
      tableName, paste0("'", x, "'"), collapse = ',')
  )[['transect_id']]
  !x %in% inDb
}
