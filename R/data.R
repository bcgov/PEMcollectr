#' Functions to produce a list of categories for PEM
#'
#' @return
#'
#' named list with code as name and description as value
#'
#' @export
#'
#' @name PEMData
map_unit <- function() {
  # TODO: check expansive list and ask about name conventions
  c('GL' = 'Glacier',
    'PN' = 'Snow Cover Water',
    'LA' = 'Lake',
    'RI' = 'River/Stream',
    'OC' = 'Ocean Exposed Land',
    'MZ' = 'Rubbly Mine Spoils',
    'LB' = 'Lava Bed',
    'RS' = 'River Sediments',
    'ES' = 'Exposed Soil',
    'LS' = 'Pond or Lake Sediments',
    'LL' = 'Landing',
    'RZ' = 'Road Surface',
    'CB' = 'Cutbank',
    'MN' = 'Moraine',
    'GP' = 'Gravel Pit',
    'TZ' = 'Tailings',
    'RN' = 'Railway Surface',
    'UR' = 'Urban',
    'AP' = 'Airport',
    'MI' = 'Open Pit Mine',
    'ST' = 'SkidTrail',
    'RP' = 'Road Permanent')
}
#' @export
#'
#' @rdname PEMData
point_type <- function() {
  c('POC' = 'POC - point of commencement',
    'POT' = 'POT - point of termination',
    'TP1' = 'TP1 - turning pt 1',
    'TP2' = 'TP2 - turning pt 2',
    'LOS' = 'LOS - line-of-sight links to a paired back-sight',
    'BS'  = 'BS - backsight')
}
#' @export
#'
#' @rdname PEMData
transition <- function() {
  c('1' = '1: homogeneous with >98% a single site series',
    '2' = '2: grading slowly towards 2nd mapunit or <10% inclusion',
    '3' = '3: Transitional between map unit concepts',
    '4' = '4: spatial gradation within segment',
    '5' = '5: 2 or more distinct site series; primary is >70%',
    '6' = '6: 2 or more distinct site series; primary is 40-70%',
    '7' = '7: 2 or more distinct site series; primary call >50% of area',
    # check for # 8 as it is not found in data
    '8' = '8: 2 distinct area in the plot; >50% is intermediate between SS1 and SS2, and remainder is SS3')
}
#' @export
#'
#' @rdname PEMData
struc_mod <- function() {
  c('C' = 'C - coniferous (> 75% of total tree cover is coniferous)',
    'B' = 'B - broadleaf (> 75% of total tree cover is broadleaf)',
    'M' = 'M - mixed (neither coniferous or broadleaf account for > 75% of total tree cover)')
}
#' @export
#'
#' @rdname PEMData
data_type <- function() {
  c('s1' = 'stage 1 sampling',
    'incidental' = 'incidental sampling')
}
#' @export
#'
#' @rdname PEMData
struc_change <- function() {
  c('1a' = '1a - sparse',
    '1b' = '1b - bryoid',
    '1c' = '1c - lichen',
    '2a' = '2a - forb-dominated',
    '2b' = '2b - graminoid-dominated',
    '2c' = '2c - aquatic',
    '2d' = '2d - dwarf shrub-dominated',
    '3a' = '3a - low shrub',
    '3b' = '3b - tall shrub',
    '4'  = '4 - pole/sampling',
    '5'  = '5 - young forest',
    '6'  = '6 - mature forest',
    '7a' = '7a - old growth',
    # not 7b not found in data
    '7b' = '7b - very old growth')
}

# mp1 <- data.table::fread('data/BEC_SER_CT_NULL_geometry.csv')
# paste0(mp1$ZONE, mp1$SUBZONE, ifelse(is.na(mp1$VARIANT), '', mp1$VARIANT), '_') |>
#   unique()
# mp1$PHASE |> unique()
# mp1$SITE_SERIES_PHASE |> unique()
