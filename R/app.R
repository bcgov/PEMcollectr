#' Run shiny app in package
#'
#' @param local
#'
#' TRUE if developing locally. FALSE uses the app from the installed package
#' location.
#'
#' @export
#'
runPEMApp <- function(local = TRUE) {
  if (isTRUE(local)) {
    appPath <- 'inst/PEMcollectr'
  } else {
    appPath <- system.file(package = 'PEMcollectr', 'PEMcolletr')
  }
  shiny::runApp(appPath)
}
