#' Run shiny app in package
#'
#' @param local
#'
#' TRUE if developing locally. FALSE uses the app from the installed package
#' location.
#'
#' @export
#'
run_PEM_app <- function(local = TRUE) {
  if (isTRUE(local)) {
    appPath <- 'inst/PEMcollectr'
  } else {
    appPath <- system.file(package = 'PEMcollectr', 'PEMcollectr')
  }
  shiny::runApp(appPath)
}
#' Wrapper to create bootswatch card
#'
#' @param title
#'
#' title of card header
#'
#' @param class
#'
#' additional classes to add
#'
#' @param headerClass
#'
#' additional classes to add to header only
#'
#' @param style
#'
#' additional styles to add to card
#'
#' @param ...
#'
#' arguments to pass to card body div
#'
#' @export
card <- function(...,
  title,
  class = '',
  style = '',
  headerClass = 'bg-primary') {
  shiny::tags$div(class = paste('card', class, collapse = ' ', sep = ' '),
    shiny::tags$div(class = paste('card-header', headerClass, collapse = ' '),
      title),
    shiny::tags$div(class = 'card-body', ...),
    style = style
  )
}
#' Make a reactive trigger
#'
#' A function to create a trigger to place inside reactive elements.
#'
#' @export
#'
#' @examples
#' triggerOne <- make_reactive_trigger()
#' # place triggerOne$depends() in reactive element to update
#' # place triggerOne$trigger() in code that should force updates
make_reactive_trigger <- function() {
  rv <- shiny::reactiveValues(a = 0)
  list(
    depend = function() {
      rv$a
      base::invisible()
    },
    trigger = function() {
      rv$a <- shiny::isolate(rv$a + 1)
    }
  )
}
#' BCGov Shiny theme
#'
#' A function to create a theme for shiny
#'
#' @export
bcgov_theme <- bslib::bs_add_rules(
  bslib::bs_theme(version = 5, bootswatch = 'materia',
    primary = '#003366',
    'progress-height' = '1rem',
    'navbar-padding-y' = '.1rem'),
  '.navbar.navbar-default {
    background-color: $primary !important;
  }
  body {
    background-color: #00000020;
  };
  ')

#' Switch Input Control
#'
#' @param inputId
#'
#' The input slot that will be used to access the value.
#'
#' @param label
#'
#' Display label for the control, or NULL for no label.
#'
#' @param value
#'
#' Initial value (TRUE or FALSE).
#'
#' @param disabled
#'
#' disable user input
#'
#' @param width
#'
#' The width of the input, e.g. '400px', or '100%'; see validateCssUnit().
#'
#' @param session
#'
#' The session object passed to function given to shinyServer. Default is
#' getDefaultReactiveDomain().
#'
#' @return
#'
#' A checkbox control that can be added to a UI definition.
#'
#' @export
#'
switchInput <- function(inputId, label, value = FALSE, disabled = FALSE,
  width = NULL) {
  value <- shiny::restoreInput(id = inputId, default = value)
  inputTag <- shiny::tags$input(id = inputId, type = "checkbox",
    role = "switch", class = "form-check-input", style='margin-right: 1rem;')
  if (!is.null(value) && value) {
    inputTag$attribs$checked <- NA
  }
  if (!is.null(disabled) && disabled) {
    inputTag$attribs$disabled <- NA
  }
  shiny::tags$div(
    class = "form-group shiny-input-container",
    style = sprintf('width: %s', shiny::validateCssUnit(width)),
    shiny::tags$div(
      class = "from-check form-switch",
      inputTag,
      shiny::tags$label(label, class = "form-check-label")
    ),
  )
}
#' @export
#'
#' @rdname switchInput
updateSwitchInput <- shiny::updateCheckboxInput

zissou <- function() {
  c('#3b99b1', '#56b29e', '#9fc095', '#eacb2b', '#e8a419', '#e87700', '#f5191c')
}
