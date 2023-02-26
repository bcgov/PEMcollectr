function(input, output, session) {
  sfObject <- geomUploadServer('geomUpload')
  submitIds <- validateTableServer('validateTable', sfObject = sfObject)
  dbWrite <- dbWriteServer('dbWrite',
    sfObject = shiny::reactive({
      sfObject()[sfObject()[['transect_id']] %in% submitIds(), ]}),
    tableName = DBI::SQL('staging.field_data_points'),
    con = con)
  validatePairsServer('validatePairs', sfObject = sfObject)
}
