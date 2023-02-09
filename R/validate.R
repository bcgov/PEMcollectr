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
  lapply(tests, function(fun, .x = x) {
    fun(.x)
    })
}
#' @rdname validate
#'
#' @export
validate_membership <- function(members) {
  function(x) {
    success <- all(vapply(x, FUN = `%in%`, FUN.VALUE = logical(1), members))
    description <- ifelse(success, '', sprintf('Values not in (%s).', x,
      paste(members, collapse = ', ')))
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
    success <- is.integer(x)
    description <- ifelse(success, '', 'Invalid integer type.')
    list(status = success, message = description)
  }
}
#' @rdname validate
#'
#' @export
validate_date <- function() {
  function(x) {
    success <- all(grepl('^[0-9]{4}-[0-9]{2}-[0-9]{2}$', x))
    description <- ifelse(success, '', 'Invalid dates.')
    list(status = success, message = description)
  }
}
#' @rdname validate
#'
#' @export
validate_time <- function(x) {
  function(x) {
    success <- all(grepl('^\\:[0-9]{2}\\:[0-9]{2}\\$', x))
    description <- ifelse(success, '', 'Invalid times.')
    list(status = success, message = description)
  }
}
#' @rdname validate
#'
#' @export
validate_missing <- function() {
  function(x) {
    success <- any(is.null(x) | is.na(x) | x == '')
    description <- ifelse(success, '', 'Missing data.')
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
    success <- st_is_valid(x)
    description <- ifelse(success, '', 'Invalid geometry.')
    list(status = success, message = description)
  }
}


validate_column <- function(dataFrame, colName) {
  .x <- dataFrame[[colName]]
  switch(
    id = '',
    placemark = validate_data(x = .x, validate_character)
  )
}
