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
#' BCGov Developer color palettes
#'
#' See \url{https://developer.gov.bc.ca/Design-System/Colour-Palette}
#'
#' @return
#'
#' vector of hex codes
#'
#' @export
#'
#' @name BCGovColors
bcgov_primary <- function() {
  c('#003366', '#fcba19', '#4c81af')
}
#' @export
#'
#' @rdname BCGovColors
bcgov_text <- function() {
  c('#313132')
}
#' @export
#'
#' @rdname BCGovColors
bcgov_links <- function() {
  c('#1A5A96')
}
bcgov_backgrounds <- function() {
  c('dark' = '#38598A', 'light' = '#F2F2F2')
}
#' @export
#'
#' @rdname BCGovColors
bcgov_components <- function() {
  c('#606060')
}
#' @export
#'
#' @rdname BCGovColors
bcgov_semantic_colors <- function() {
  c('error' = '#D8292F', 'success' = '#2E8540')
}
#' BCGov Shiny theme
#'
#' A function to create a theme for shiny
#'
#' @export
bcgov_theme <- bslib::bs_add_rules(
  bslib::bs_theme(version = 5, bootswatch = 'materia',
    primary = bcgov_primary()[1],
    'link-color' = sprintf('%s', bcgov_links()[1]),
    'progress-height' = '1rem',
    'navbar-padding-y' = '.1rem',
    'body-color' = bcgov_text()[1]
    ),
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
#' @param class
#'
#' additional classes for form.control
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
  width = NULL, class = '') {
  value <- shiny::restoreInput(id = inputId, default = value)
  inputTag <- shiny::tags$input(id = inputId, type = 'checkbox',
    role = 'switch', class = 'form-check-input', style = 'margin-right: 1rem;')
  if (!is.null(value) && value) {
    inputTag$attribs$checked <- NA
  }
  if (!is.null(disabled) && disabled) {
    inputTag$attribs$disabled <- NA
  }
  shiny::tags$div(
    class = sprintf('form-group shiny-input-container %s', class),
    style = sprintf('width: %s', shiny::validateCssUnit(width)),
    shiny::tags$div(
      class = 'from-check form-switch',
      inputTag,
      shiny::tags$label(label, class = 'form-check-label')
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
