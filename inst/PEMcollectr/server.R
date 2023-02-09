library(shiny)
function(input, output, session) {
  sfObject <- geomUploadServer('geomUpload')
  output$dataValidation <- shiny::renderTable({
    shiny::req(sfObject())
    sf::st_drop_geometry(sfObject())
  })
}
