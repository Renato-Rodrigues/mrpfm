#' Convert OECD Environmental Policy Stringency (EPS) data to the ISO3 country set
#'
#' @description
#' Fills the 40 EPS-reporting countries out to the full ISO3 universe madrat works in.
#'
#' Uncovered countries are left as \code{NA}, deliberately, and \strong{not} filled
#' with zero. EPS is a measure of how stringent environmental policy is, not of
#' whether any exists; a country the OECD never scored is unmeasured, not unregulated.
#' Zero-filling would place ~200 countries at the bottom of the distribution and drag
#' any correlation computed against CAPMF toward a number that describes the fill rule
#' rather than the data.
#'
#' @param x magpie object from \code{\link{readOECD_EPS}}.
#'
#' @return A [`magpie`][magclass::magclass] object over the full ISO3 country set,
#'   `NA` where EPS does not report.
#'
#' @author Renato Rodrigues
#'
#' @importFrom madrat toolCountryFill
#'
#' @seealso \code{\link{readOECD_EPS}}, \code{\link{downloadOECD_EPS}}
#'
#' @export
convertOECD_EPS <- function(x) {   # nolint: object_name_linter
  madrat::toolCountryFill(x, fill = NA, verbosity = 2)
}
