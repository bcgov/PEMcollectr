function(input, output, session) {
  sfObject <- geomUploadServer('geomUpload')
  submitIds <- validateTableServer('validateTable', sfObject = sfObject,
    success = dbWrite, con = con)
  dbWrite <- dbWriteServer('dbWrite',
    sfObject = shiny::reactive({
      sfObject()[sfObject()[['transect_id']] %in% submitIds(), ]}),
    tableName = shiny::reactive({
      staging_tables()[[guess_geometry_type(sfObject())]]
      }),
    con = con)
  validatePairsServer('validatePairs', sfObject = sfObject, success = dbWrite,
    con = con)
  mapServer('dcMap', con = con, sfObject = sfObject, success = dbWrite)
  photoId <- dbPhotoServer('photo', con = con)
  dbShowPhotoServer('showPhoto', con = con, selectedPoint = photoId)
}
