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
  style = '') {
  shiny::tags$div(class = paste('card', class, collapse = ' ', sep = ' '),
    shiny::tags$div(class = 'card-header', title),
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
