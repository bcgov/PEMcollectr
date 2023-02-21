function(input, output, session) {
  sfObject <- geomUploadServer('geomUpload')
  submitIds <- validateTableServer('validateTable', sfObject = sfObject)
  dbWrite <- dbWriteServer('dbWrite',
    sfObject = shiny::reactive({
      sfObject()[sfObject()[['transect_id']] %in% submitIds(), ]}))
}
