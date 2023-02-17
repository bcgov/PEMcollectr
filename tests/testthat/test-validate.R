test_that("Validation works", {
  validate_complex <- function() {
    function(x) {
      success <- is.complex(x)
      description <- ifelse(success, '', 'Invalid complex type.')
      list(status = success, message = description)
    }
  }
  validate_data(complex(rep(1, 2), c(1, -1)), validate_complex())
  validate_data(x = c('A', 'B'), validate_character(), validate_integer())
  validate_membership(c('A', 'B'))(c('A', 'C'))
  validate_date()(c('2023-01-01', '01'))
  validate_time()(c('23:59:59', '59'))
})
