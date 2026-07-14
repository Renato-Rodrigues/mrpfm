# nolint start
#' Convert OECD value added by activity to the full country universe
#'
#' Fills the OECD country sample up to the madrat ISO3 universe with `NA` (no
#' imputation — a country without national-accounts data has no GDP-share weight,
#' and [`calcPolicyStringency()`] then falls back to equal weights for that cell,
#' flagged, per the T2 no-imputation rule).
#'
#' @param x magpie object from [`readOECDValueAdded()`].
#' @returns A [`magpie`][magclass::magclass] object covering all madrat countries.
#' @author Renato Rodrigues
#' @importFrom madrat toolCountryFill
#' @export
convertOECDValueAdded <- function(x) {
  madrat::toolCountryFill(x, fill = NA, verbosity = 2)
}
# nolint end
