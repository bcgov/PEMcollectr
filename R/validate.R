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
validate_membership <- function(members) {
  function(x) {
    isValid <- x %in% members
    success <- all(isValid)
    description <- ifelse(success, '',
      sprintf('Invalid categories: %d records (%.2f%%) not in set',#(%s).',
        sum(!isValid, na.rm = TRUE),
        100 * sum(!isValid, na.rm = TRUE) / length(isValid)
        #,paste(members, collapse = ', ')
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
validate_time <- function() {
  function(x) {
    isValid <- grepl('^\\:[0-9]{2}\\:[0-9]{2}\\$', x)
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
    isValid <- is.na(x) | x == ''
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
    success <- identical(sum(x %in% string, na.rm = TRUE), times)
    description <- ifelse(success, '', sprintf('%s must appear %d time(s).',
      string, times))
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
    validPattern <- '[A-Z]{3,4}[a-z]{2}[0-9]*\\_[0-9]{1}\\.[0-9]{1}_[0-9]+\\_c*[A-Za-z]+'
    isValid <- grepl(validPattern, x)
    success <- all(isValid)
    description <- ifelse(success, '',
      make_format_message(title = 'Invalid times', isValid = isValid,
        specification = 'AAAaa#_#.#_#_#_AAA'))
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
#' @rdname validate
#'
#' @export
validate_points_column <- function(x, colName) {
  switch(colName,
    id = validate_data(x = x, validate_nothing()),
    order = validate_data(x = x, validate_integer()),
    date_ymd = validate_data(x = x, validate_date()),
    time_hms = validate_data(x = x, validate_time()),
    transect_id = validate_data(x = x, validate_nothing()),
    observer = validate_data(x = x, validate_missing()),
    point_type = validate_data(x = x,
      validate_contains(point_type()['POC'], times = 1L),
      validate_contains(point_type()['POT'], times = 1L),
      validate_contains(point_type()['TP1'], times = 1L),
      validate_contains(point_type()['TP2'], times = 1L)
    ),
    mapunit1 = validate_data(x = x, validate_membership(members = map_unit())),
    mapunit2 = validate_data(x = x, validate_membership(members = map_unit())),
    transition =
      validate_data(x = x, validate_membership(members = transition())),
    struc_change =
      validate_data(x = x, validate_membership(members = struc_change())),
    struc_mod =
          validate_data(x = x, validate_membership(members = struc_mod())),
    edatope = validate_data(x = x, validate_character()),
    comments = validate_data(x = x, validate_character(),
      validate_char_length(255)),
    photos = validate_data(x = x, validate_character()),
    data_type =
      validate_data(x = x, validate_membership(members = data_type())),
    geom = validate_data(x = x, #validate_geometry(),
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
validate_PEM_data <- function(dataFrame, f) {
  Map(f = f,
    x = dataFrame,
    colName = names(dataFrame)
  )
}
#' @rdname validate
#'
#' @export
validate_tracklog_column <- function(x, colName) {
  switch(colName,
    id = validate_data(x = x, validate_nothing()),
    transect_id = validate_data(x = x, validate_nothing()),
    date_ymd = validate_data(x = x, validate_date()),
    time_hms = validate_data(x = x, validate_time()),
    photos = validate_data(x = x, validate_character()),
    comments = validate_data(x = x, validate_character(),
      validate_char_length(255)),
    data_type =
      validate_data(x = x, validate_membership(members = data_type())),
    geom = validate_data(x = x, #validate_geometry(),
      validate_geometry_type(geometryType = 'LINESTRING')),
    err_column_name(colName)
  )
}
#' @rdname validate
#'
#' @export
err_column_name <- function(colName) {
  list(status = 0L,
    message = sprintf('%s is not a valid column name.', colName))
}
#' @param transectPoints
#'
#' sf object with transect field points data
#'
#' @rdname validate
#'
#' @export
validate_field_points_data <- function(transectPoints) {
  lapply(split(transectPoints, transectPoints[['transect_id']]),
    validate_PEM_data,
    f = validate_points_column)
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
#' @export
#'
#' @name validateTable
#'
validateTableUi <- function(id) {
  ns <- shiny::NS(id)
  shiny::tags$div(
    reactable::reactableOutput(ns('dataValidation'))
    )
}
#' @export
#'
#' @rdname validateTable
validateTableServer <- function(id, sfObject) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      validationResults <- shiny::reactive({
        data.table::rbindlist(
          lapply(validate_field_points_data(sfObject()),
            data.table::as.data.table),
          fill = TRUE, idcol = 'id')
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
          defaultColDef = reactable::colDef(
            cell = function(value) {
              if (is.null(value)) {
                return(NULL)
              }
              if (value != '') {
                shiny::tags$div(style = 'display: inline-block; padding: 0.125rem 0.75rem;
          border-radius: 5px; font-weight: 600; font-size: 0.75rem;
          background: hsl(350, 70%, 90%); color: hsl(350, 45%, 30%);',
                  value)
              }
              #list(background = ifelse(value == '', blue, red))
            }),
          #rowStyle = list(background = "rgba(0, 0, 0, 0.05)"),
          selection = "multiple",
          bordered = TRUE,
          striped = TRUE,
          compact = TRUE
        )
      })
      return(submitIds)
    }
  )
}
