#' Validation tests for vectors
#'
#' @param x
#'
#' vector of values to test
#'
#' @param members
#'
#' vector of values to test membership
#'
#' @param exact
#'
#' TRUE matches the exact times arg and FALSE is >= times
#'
#' @param dataFrame
#'
#' data.frame to validate
#'
#' @param colName
#'
#' character vector size 1 of column to validate
#'
#' @param optional
#'
#' TRUE/FALSE for whether missing values are acceptable
#'
#' @param ...
#'
#' validation functions for x. See example for writing custom tests.
#'
#' @return
#'
#' list with status of pass/fail and message of error
#'
#' @name validate
#'
#' @export
#'
#' @examples
#'
#' # usage
#' validate_data(x = c('A', 'B'), validate_character(), validate_integer())
#'
#' # custom validation function
#' validate_complex <- function() {
#' function(x) {
#'   success <- is.complex(x)
#'   description <- ifelse(success, '', 'Invalid complex type.')
#'   list(status = success, message = description)
#' }
#' }
#' validate_data(complex(rep(1, 2), c(1, -1)), validate_complex())
validate_data <- function(x, ...) {
  tests <- list(...)
  stopifnot(any(!is.function(tests)))
  collect_validations(lapply(tests, function(fun, .x = x) {
    fun(.x)
    }))
}
collect_validations <- function(validations) {
  # noErrors <- sum(!vapply(validations, FUN = getElement,
  # FUN.VALUE = logical(1),
  #   name = 'status'))
  allMessages <- vapply(validations, FUN = getElement, FUN.VALUE = character(1),
    name = 'message')
  #list(
    #errors = noErrors,
  c(
    messages = paste0(allMessages[allMessages != ''], collapse = '; ')
  )
}
#' @rdname validate
#'
#' @export
validate_membership <- function(members, optional = FALSE) {
  function(x) {
    isValid <- x %in% members
    invalidSet <- unique(stats::na.omit(x[!isValid]))
    invalidSet <- ifelse(length(invalidSet) < 1, 'missing',
      sprintf('not in set [%s]', paste(invalidSet, collapse = ', ')))
    if (isTRUE(optional)) {
      isValid <- isValid | is.na(x)
    }
    success <- all(isValid)
    description <- ifelse(success, '',
      sprintf('Invalid categories: %d records (%.2f%%) %s.',
        sum(!isValid, na.rm = TRUE),
        100 * sum(!isValid, na.rm = TRUE) / length(isValid),
        invalidSet
        ))
    list(status = success, message = description)
  }
}
#' @rdname validate
#'
#' @export
validate_character <- function() {
  function(x) {
    success <- is.character(x)
    description <- ifelse(success, '', 'Invalid character type.')
    list(status = success, message = description)
  }
}
#' @rdname validate
#'
#' @export
validate_float <- function() {
  function(x) {
    success <- is.numeric(x)
    description <- ifelse(success, '', 'Invalid numeric type.')
    list(status = success, message = description)
  }
}
#' @rdname validate
#'
#' @export
validate_integer <- function() {
  function(x) {
    if (is.numeric(x)) {
      success <- is.wholenumber(x)
    } else {
      success <- is.integer(x)
    }
    description <- ifelse(success, '', 'Invalid integer type.')
    list(status = success, message = description)
  }
}
is.wholenumber <- function(x, tol = .Machine$double.eps^0.5) {
  all(abs(x - round(x)) < tol)
}
#' @rdname validate
#'
#' @export
validate_date <- function() {
  function(x) {
    isValid <- grepl('^[0-9]{4}-[0-9]{2}-[0-9]{2}$', x)
    success <- all(isValid)
    description <- ifelse(success, '',
      make_format_message(title = 'Invalid dates', isValid = isValid,
        specification = 'YYYY-MM-DD'))
    list(status = success, message = description)
  }
}
#' @rdname validate
#'
#' @export
validate_time <- function(optional = FALSE) {
  function(x) {
    isValid <- grepl('^\\:[0-9]{2}\\:[0-9]{2}\\$', x)
    if (isTRUE(optional)) {
      isValid <- isValid | is.na(x)
    }
    success <- all(isValid)
    description <- ifelse(success, '',
      make_format_message(title = 'Invalid times', isValid = isValid,
        specification = 'HH:MM:SS'))
    list(status = success, message = description)
  }
}
#' @rdname validate
#'
#' @export
validate_missing <- function() {
  function(x) {
    isValid <- is.na(x)
    success <- any(isValid)
    description <- ifelse(success, '',
      sprintf('Missing records:\n%d records (%.2f%%) missing.',
        sum(!isValid, na.rm = TRUE),
        100 * sum(!isValid, na.rm = TRUE) / length(isValid)))
    list(status = success, message = description)
  }
}
#' @rdname validate
#'
#' @export
validate_nothing <- function() {
  function(x) {
    list(status = TRUE, message = '')
  }
}
#' @rdname validate
#'
#' @export
validate_geometry <- function() {
  function(x) {
    success <- sf::st_is_valid(x)
    description <- ifelse(success, '', 'Invalid geometry.')
    list(status = success, message = description)
  }
}
#' @param geometryType
#'
#' geometry type to validate
#'
#' @rdname validate
#'
#' @export
validate_geometry_type <- function(geometryType) {
  function(x) {
    isValid <- sf::st_geometry_type(x) == geometryType
    success <- all(isValid)
    description <- ifelse(success, '',
      make_format_message(title = 'Invalid geometry', isValid = isValid,
        specification = geometryType))
    list(status = success, message = description)
  }
}
#' @param string
#'
#' character to vector to match
#'
#' @param times
#'
#' integer exact number of expected matches
#'
#' @rdname validate
#'
#' @export
validate_contains <- function(string, times = 1L, exact = FALSE) {
  function(x) {
    if (isTRUE(exact)) {
      success <- sum(x %in% string, na.rm = TRUE) == times
      description <- ifelse(success, '', sprintf('%s must appear %d time(s).',
        string, times))
    } else {
      success <- sum(x %in% string, na.rm = TRUE) >= times
      description <- ifelse(success, '',
        sprintf('%s must appear at least %d time(s).',
        string, times))
    }
    list(status = success, message = description)
  }
}
#'
#' @param maxLength
#'
#' integer maximum allowable character length
#'
#' @rdname validate
#'
#' @export
validate_char_length <- function(maxLength) {
  function(x) {
    x[is.na(x)] <- ''
    isValid <- nchar(x) <= maxLength
    success <- all(isValid)
    description <- ifelse(success, '',
      make_format_message(title = 'Invalid times', isValid = isValid,
        specification = sprintf('Max character length: %d', maxLength)))
    list(status = success, message = description)
  }
}
#' @rdname validate
#'
#' @export
validate_transect_id <- function() {
  function(x) {
    if ( all(x == 'incidental')) {
      return(list(status = TRUE, message = ''))
    }
    transectParts <- data.table::tstrsplit(x, '_')
    if (length(transectParts) != 4) {
      return(list(status = TRUE, message = 'Invalid transect_id format'))
    }
    names(transectParts) <- sprintf('part%d', 1:4)
    isValidMapUnit <- validate_membership(
      members = c(sub('\\/.*', '', map_unit_veg()), map_unit_non_veg())
      )(transectParts[['part1']])
    isValidTransectLocation <- validate_membership(
      members = transect_location())(transectParts[['part4']])
    isValidSegments <- validate_float()(as.numeric(transectParts[['part2']]))
    isValidTotal <- validate_integer()(as.integer(transectParts[['part3']]))
    validResults <- list(isValidMapUnit, isValidTransectLocation, isValidSegments,
      isValidTotal)
    success <- all(vapply(validResults, FUN = getElement,
      FUN.VALUE = logical(1), name = 'status'))
    description <- collect_validations(validResults)
    list(status = success, message = description)
  }
}
make_format_message <- function(title, isValid, specification) {
  sprintf('%s:
%d records (%.2f%%) with incorrect format (%s) or missing.',
    title, sum(!isValid, na.rm = TRUE),
    100 * sum(!isValid, na.rm = TRUE) / length(isValid),
    specification)
}
#' @param mapunit1
#'
#' first map unit
#'
#' @param mapunit2
#'
#' second map unit
#'
#' @rdname validate
#'
#' @export
validate_map_unit_change <- function(mapunit1, mapunit2) {
  function(x) {
    # convert to "NA" for comparisons as "!=" will return NA
    isValid <- sprintf('%s', mapunit1) != sprintf('%s', mapunit2)
    success <- all(isValid)
    description <- ifelse(success, '', 'Invalid pair:
mapunit1 is the same as mapunit2.')
    list(status = success, message = description)
  }
}
#' @rdname validate
#'
#' @export
validate_map_unit_transition <- function(mapunit2) {
  function(x) {
    isValid <- (!is.na(mapunit2) & !is.na(x)) |
      (is.na(mapunit2) & is.na(x))
    success <- all(isValid)
    description <- ifelse(success, '', 'Missing pair:
transition must be included if mapunit2 is specified.')
    list(status = success, message = description)
  }
}
#' @rdname validate
#'
#' @export
validate_points_column <- function(dataFrame, colName) {
  if (all(dataFrame[['transect_id']] == 'incidental')) {
    observerVal <- validate_data(x = dataFrame[[colName]],
      validate_nothing())
    pointTypeVal <- validate_data(x = dataFrame[[colName]],
      validate_nothing())
  } else {
    pointTypeVal <- validate_data(x = dataFrame[[colName]],
      validate_contains(point_type()['POC'], times = 1L),
      validate_contains(point_type()['POT'], times = 1L),
      validate_contains(point_type()['TP1'], times = 1L),
      validate_contains(point_type()['TP2'], times = 1L))
    observerVal <- validate_data(x = dataFrame[[colName]],
      validate_missing())
  }
  switch(colName,
    id = validate_data(x = dataFrame[[colName]],
      validate_nothing()),
    order = validate_data(x = dataFrame[[colName]],
      validate_integer()),
    date_ymd = validate_data(x = dataFrame[[colName]],
      validate_date()),
    time_hms = validate_data(x = dataFrame[[colName]],
      validate_time(optional = TRUE)),
    transect_id = validate_data(x = dataFrame[[colName]],
      validate_transect_id()),
    observer = observerVal,
    point_type = pointTypeVal,
    mapunit1 = validate_data(x = dataFrame[[colName]],
      validate_membership(members = c(map_unit_veg(), map_unit_non_veg()))),
    mapunit2 = validate_data(x = dataFrame[[colName]],
      validate_membership(members = c(map_unit_veg(), map_unit_non_veg()),
        optional = TRUE)),
    transition =
      validate_data(x = dataFrame[[colName]],
        validate_map_unit_change(dataFrame[['mapunit1']],
          dataFrame[['mapunit2']]),
        validate_map_unit_transition(dataFrame[['mapunit2']]),
        validate_membership(members = transition(), optional = TRUE)),
    struc_stage =
      validate_data(x = dataFrame[[colName]],
        validate_membership(members = struc_stage())),
    struc_mod =
      validate_data(x = dataFrame[[colName]],
        validate_membership(members = struc_mod())),
    edatope = validate_data(x = dataFrame[[colName]],
      validate_character(),
      validate_membership(members = edatope(), optional = TRUE)),
    comments = validate_data(x = dataFrame[[colName]],
      validate_character(),
      validate_char_length(255)),
    photos = validate_data(x = dataFrame[[colName]],
      validate_character()),
    data_type =
      validate_data(x = dataFrame[[colName]],
        validate_membership(members = data_type())),
    geom = validate_data(x = dataFrame[[colName]],
      #validate_geometry(),
      validate_geometry_type(geometryType = 'POINT')),
    validate_data(x = colName, err_column_name)
  )
}
#' @param f
#'
#' a column validation function for PEM data
#'
#' @rdname validate
#'
#' @export
validate_PEM_data <- function(dataFrame, f, progress, n = 1) {
  if (!missing(progress)) {
    progress$inc(1 / n)
  }
  stats::setNames(lapply(names(dataFrame), f, dataFrame = dataFrame),
    names(dataFrame))
}
#' @param transectPoints
#'
#' sf object with transect field points data
#'
#' @rdname validate
#'
#' @export
validate_field_points_data <- function(transectPoints, progress) {
  transectPoints <- split(transectPoints, transectPoints[['transect_id']])
  if (!missing(progress)) {
    lapply(transectPoints,
      validate_PEM_data,
      f = validate_points_column,
      progress = progress, n = length(transectPoints))
  } else {
    lapply(transectPoints,
      validate_PEM_data,
      f = validate_points_column)
  }
}
#' @rdname validate
#'
#' @export
validate_tracklog_column <- function(dataFrame, colName) {
  switch(colName,
    id = validate_data(x = dataFrame[[colName]], validate_nothing()),
    transect_id = validate_data(x = dataFrame[[colName]], validate_nothing()),
    date_ymd = validate_data(x = dataFrame[[colName]], validate_date()),
    time_hms = validate_data(x = dataFrame[[colName]], validate_time()),
    photos = validate_data(x = dataFrame[[colName]], validate_character()),
    comments = validate_data(x = dataFrame[[colName]], validate_character(),
      validate_char_length(255)),
    data_type =
      validate_data(x = dataFrame[[colName]], validate_membership(members = data_type())),
    geom = validate_data(x = dataFrame[[colName]], #validate_geometry(),
      validate_geometry_type(geometryType = 'LINESTRING')),
    err_column_name(colName)
  )
}
#' @param transectLines
#'
#' sf object of tracklog line data
#'
#' @rdname validate
#'
#' @export
validate_field_tracklog_data <- function(transectLines, progress, n = 1) {
  transectLines <- split(transectLines, transectLines[['transect_id']])
  if (!missing(progress)) {
    progress$inc(1 / n)
  }
  lapply(transectLines,
    validate_PEM_data,
    f = validate_tracklog_column)
}
#' @rdname validate
#'
#' @export
err_column_name <- function(colName) {
  list(status = FALSE,
    message = sprintf('%s is not a valid column name.', colName))
}
#' @param n
#'
#' length
#'
#' @param distinct
#'
#' TRUE/FALSE should test be based on unique values
#'
#' @rdname validate
#'
#' @export
validate_length <- function(n, distinct = TRUE) {
  function(x) {
    distinctText <- ' '
    if (isTRUE(distinct)) {
      x <- unique(x)
      distinctText <- ' unique '
    }
    noItems <- length(x)
    success <- noItems == n
    description <- ifelse(success, '',
      sprintf('Expected %d%svalues in group and %d were found', n, distinctText,
        noItems))
    list(status = success, message = description)
  }
}
#' @param transectIds
#'
#' vector of transect ids
#'
#' @rdname validate
#'
#' @export
validate_sample_pairs <- function(transectIds) {
  transectParts <- data.table::tstrsplit(transectIds, '_')
  transectParts <- expand_list(x = transectParts, n = 4,
    times = length(transectIds))
  lapply(split(transectParts[[4]], paste(transectParts[[1]], transectParts[[2]],
    transectParts[[3]], sep = '_')), validate_data,
    validate_contains(string = 'cLHS', times = 1L, exact = TRUE),
    validate_length(n = 2, distinct = TRUE))
}
expand_list <- function(x, n = 4, times) {
  if (length(x) > (n - 1)) {
    x
  } else {
    x <- append(x, list(rep(NA, times)))
    expand_list(x, n = n, times = times)
  }
}
#' Shiny Module to validate and report
#'
#' @param id
#'
#' namespace id
#'
#' @param sfObject
#'
#' reactive object returning sf data.frame
#'
#' @param success
#'
#' reactive trigger when data is successfully uploaded
#'
#' @param con
#'
#' connection to postgres database
#'
#' @export
#'
#' @name validateTable
#'
validateTableUi <- function(id) {
  ns <- shiny::NS(id)
  shiny::tags$div(
    shiny::uiOutput(ns('dbFilterUi')),
    reactable::reactableOutput(ns('dataValidation'))
    )
}
#' @export
#'
#' @rdname validateTable
validateTableServer <- function(id, sfObject, success, con) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      dataInDb <- shiny::reactive({
        shiny::req(sfObject())
        geometryType <- guess_geometry_type(sfObject())
        stagingTable <- staging_tables()[[geometryType]]
        transectsTable <- transects_tables()[[geometryType]]
        tryCatch(is_in_db(con = con, transectIds =
            sfObject()[['transect_id']], observers = sfObject()[['observer']],
          stagingTable = stagingTable, transectsTable = transectsTable),
          error = identity)
      })
      output$dbFilterUi <- shiny::renderUI({
        shiny::req(!inherits(dataInDb(), 'error'))
        uniqueIds <- unique(as.data.frame(sfObject()[dataInDb(), ])[,
          c('transect_id', 'observer')])
        if (nrow(uniqueIds) < 1) {
          return(NULL)
        }
        shiny::tags$div(class='mb-2',
          shiny::tags$h6('Data already submitted for (transect_id-observer):'),
          paste(sprintf('%s-%s', uniqueIds[[1]], uniqueIds[[2]]),
            collapse = '; ')
        )
      })
      validationResults <- shiny::reactive({
        shiny::req(sfObject())
        success$depend()
        geometryType <- guess_geometry_type(sfObject())
        #stagingTable <- staging_tables()[[geometryType]]
        #transectsTable <- transects_tables()[[geometryType]]
        if (inherits(x = dataInDb(), 'error')) {
          shiny::showNotification(
            ui = 'Network connection error: database is unavailable. Please try again.',
            duration = 5, type = 'error')
          return(NULL)
        }
        sfObject <- sfObject()[!dataInDb(), ]
        shiny::req(nrow(sfObject) > 0)
        if (geometryType %in% 'POINT') {
          progress <- shiny::Progress$new(session)
          on.exit(progress$close())
          progress$set(value = .01, message = 'Validation in progress...')
          results <- data.table::rbindlist(
            lapply(validate_field_points_data(sfObject, progress = progress),
              data.table::as.data.table),
            fill = TRUE, idcol = 'id')
          hardConstraints <- c('transect_id', 'point_type',
            #'transition', 'struc_mod', 'struc_stage', 'observer',
            'date_ymd', 'data_type',
            'geom')
          results[['Valid']] <- vapply(
            split(results[ , hardConstraints],
              results[['id']]), FUN = function(x) {
              all(x == '')
            }, FUN.VALUE = logical(1))
        } else if (geometryType %in% c('LINESTRING', 'MULTILINESTRING')) {
          progress <- shiny::Progress$new(session)
          on.exit(progress$close())
          progress$set(value = .01, message = 'Validation in progress...')
          results <- data.table::rbindlist(
            lapply(validate_field_tracklog_data(sfObject, progress = progress),
              data.table::as.data.table),
            fill = TRUE, idcol = 'id')
          hardConstraints <- c('transect_id', 'date_ymd', 'data_type',
            'geom')
          results[['Valid']] <- vapply(
            split(results[ , hardConstraints], results[['id']]),
            FUN = function(x) {
                all(x == '')
              }, FUN.VALUE = logical(1))
        } else {
          shiny::showNotification(
            ui = 'Unsupported geometry type or mixed geometry',
            duration = NULL, type = 'error')
          return(NULL)
        }
        results[order(results[['Valid']], decreasing = TRUE), ]
      })
      selected <- shiny::reactive({
        reactable::getReactableState('dataValidation', 'selected')
      })
      submitIds <- shiny::reactive({
        shiny::req(sfObject())
        validationResults()[selected(), 'id']
      })
      output$dataValidation <- reactable::renderReactable({
        shiny::req(validationResults())
        reactable::reactable(validationResults(),
          columns = list(
            .selection = reactable::colDef(
              sticky = 'left',
              headerClass = 'hide-checkbox',
              class = ifelse(validationResults()[['Valid']], '', 'hide-checkbox')
            ),
            id = reactable::colDef(
              sticky = 'left',
              cell = function(value) {
              shiny::tags$div(class = 'cell-id', value)
              }
            ),
            mapunit1 = reactable::colDef(cell = function(value) {
              if (value != '') {
                shiny::tags$div(class = 'cell-warning', value)
              }
            }
            ),
            mapunit2 = reactable::colDef(cell = function(value) {
              if (value != '') {
                shiny::tags$div(class = 'cell-warning', value)
              }
            }
            ),
            edatope = reactable::colDef(cell = function(value) {
              if (value != '') {
                shiny::tags$div(class = 'cell-warning', value)
              }
            }
            ),
            time_hms = reactable::colDef(cell = function(value) {
              if (value != '') {
                shiny::tags$div(class = 'cell-warning', value)
              }
            }
            ),
            Valid =  reactable::colDef(cell = function(value) {
              if (isTRUE(value)) {
                shiny::tags$div(class = 'cell-success', value)
              } else {
                shiny::tags$div(class = 'cell-error', value)
              }
              })
          ),
          defaultColDef = reactable::colDef(
            cell = function(value) {
              if (is.null(value)) {
                return(NULL)
              }
              if (value != '') {
                shiny::tags$div(class = 'cell-error', value)
              }
              #list(background = ifelse(value == '', blue, red))
            }),
          #rowStyle = list(background = "rgba(0, 0, 0, 0.05)"),
          theme = reactable::reactableTheme(borderWidth = '1px',
            borderColor = '#00000040'),
          selection = "multiple",
          pageSizeOptions = c(5, 10, 25, 50),
          defaultPageSize = 5,
          bordered = TRUE,
          resizable = TRUE,
          #striped = TRUE,
          compact = TRUE
        )
      })
      return(submitIds)
    }
  )
}
#' Shiny Module to validate transect pairs
#'
#' @param id
#'
#' namespace id
#'
#' @param sfObject
#'
#' reactive object returning sf data.frame
#'
#' @param success
#'
#' reactive trigger when data is successfully uploaded
#'
#' @param con
#'
#' connection to postgres database
#'
#' @export
#'
#' @name validateTable
#'
validatePairsUi <- function(id) {
  ns <- shiny::NS(id)
  shiny::tags$div(
    reactable::reactableOutput(ns('pairValidation'))
  )
}
#' @export
#'
#' @rdname validateTable
validatePairsServer <- function(id, sfObject, success, con) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      validationResults <- shiny::reactive({
        shiny::req(sfObject())
      })
      output$pairValidation <- reactable::renderReactable({
        shiny::req(sfObject())
        success$depend()
        geometryType <- guess_geometry_type(sfObject())
        stagingTable <- staging_tables()[[geometryType]]
        transectsTable <- transects_tables()[[geometryType]]
        dbTransectIds <- select_distinct_transect_ids(con= con,
          stagingTable = stagingTable, transectsTable = transectsTable)
        transectIds <- c(sort(unique(stats::na.omit(sfObject()[['transect_id']])
          )))
        transectIds <- transectIds[transectIds != 'incidental']
        transectIds <- unique(c(transectIds, dbTransectIds))
        validatePairs <- vapply(
          validate_sample_pairs(transectIds = transectIds), FUN = getElement,
          FUN.VALUE = character(1), name = 'messages')
        reactable::reactable(
          data.table::data.table(id = names(validatePairs),
            Pairs = validatePairs,
            Valid = validatePairs == ''),
          columns = list(
            id = reactable::colDef(
              sticky = 'left',
              cell = function(value) {
                shiny::tags$div(class = 'cell-id', value)
              }
            ),
            Pairs = reactable::colDef(
              cell = function(value) {
                if (is.null(value)) {
                  return(NULL)
                }
                if (value != '') {
                  shiny::tags$div(class = 'cell-error', value)
                }
              }),
            Valid =  reactable::colDef(cell = function(value) {
              if (isTRUE(value)) {
                shiny::tags$div(class = 'cell-success', value)
              } else {
                shiny::tags$div(class = 'cell-error', value)
              }
            })
        ),
        theme = reactable::reactableTheme(borderWidth = '1px',
          borderColor = '#00000040'),
        pageSizeOptions = c(5, 10, 25, 50),
        defaultPageSize = 5,
        bordered = TRUE,
        #striped = TRUE,
        resizable = TRUE,
        compact = TRUE
        )
      })
    }
  )
}
