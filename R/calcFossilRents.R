#' Calculate fossil rents
#'
#' Oil + natural gas + coal rents, \% of GDP (World Bank WDI, [readWDIRents()]), per country and year: the sum
#' of the three (missing when all three are), gaps interpolated linearly, held at the last reported value
#' up to `lastYear` (the series ends a few years before the present) and before the first one. pfm uses it
#' as `log(1 + rents)` for the size of the frontier's slack (design note 0009 section 8.2).
#'
#' @param lastYear The year the last reported value is held to.
#' @return A list for [madrat::calcOutput()]: `x` `[iso3c, year, "Fossil Rents"]` (\% of GDP), weighted by
#'   SSP2 population for aggregation (as [calcVDem()]; uniform when it is unavailable).
#' @author Renato Rodrigues
#' @importFrom madrat readSource calcOutput
#' @importFrom magclass new.magpie getItems getYears setNames
calcFossilRents <- function(lastYear = 2025) {
  r <- readSource("WDIRents", convert = TRUE)
  yrs <- seq(min(getYears(r, as.integer = TRUE)), max(lastYear, max(getYears(r, as.integer = TRUE))))
  a <- as.array(r)
  tot <- apply(a, c(1, 2), function(v) if (all(!is.finite(v))) NA_real_ else sum(v, na.rm = TRUE))
  have <- as.integer(sub("^y", "", colnames(tot)))
  fill <- function(v) {
    ok <- which(is.finite(v))
    if (!length(ok)) return(rep(NA_real_, length(yrs)))
    stats::approx(have[ok], v[ok], xout = yrs, rule = 2)$y   # interpolate inside, hold outside
  }
  out <- t(apply(tot, 1, fill))
  x <- new.magpie(rownames(tot), yrs, "Fossil Rents", fill = NA)
  x[, , 1] <- out
  # SSP2 population, as calcVDem (the panels are at country resolution, so the weight rarely acts)
  w <- tryCatch({
    p <- suppressWarnings(calcOutput("Population", scenario = "SSP2", aggregate = FALSE))
    p <- magclass::time_interpolate(p[, , if ("SSP2" %in% magclass::getNames(p)) "SSP2" else 1], yrs,
                                    integrate_interpolated_years = TRUE, extrapolation_type = "constant")
    setNames(magclass::collapseNames(p)[getItems(x, dim = 1), yrs, ], "Fossil Rents")
  }, error = function(e) NULL)
  if (is.null(w)) { w <- x; w[, , ] <- 1 }
  list(x = x, weight = w, unit = "% of GDP",
       description = "Oil, natural gas and coal rents, % of GDP (World Bank WDI); gaps interpolated, last value held")
}
