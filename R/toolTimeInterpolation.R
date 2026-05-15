#' @title toolTimeInterpolation
#' @description interpolates time series data backward
#'
#' @param x magpie object
#' @param interpolatedYears years to be interpolated
#' @return Returns the combined magpie object
#' @author Renato Rodrigues
#'
#' @importFrom magclass mbind setNames add_dimension getYears time_interpolate getItems new.magpie
#' @importFrom stats approx
#' @importFrom madrat calcOutput toolGetMapping
#' @export
#'
toolTimeInterpolation <- function(x, interpolatedYears) {
  regs <- magclass::getItems(x, dim = 1)
  vars <- magclass::getItems(x, dim = 3)
  out <- new.magpie(
    cells_and_regions = regs,
    years = interpolatedYears,
    names = vars,
    fill = NA
  )
  for (v in vars) {
    for (r in regs) {
      # 1. FULL year range (no filtering)
      yrsAll <- getYears(x[r, , v], as.integer = TRUE)
      valsAll <- as.numeric(x[r, , v])
      if (all(is.na(valsAll))) next
      valid <- which(!is.na(valsAll))
      if (length(valid) < 2) next
      validYears <- yrsAll[valid]
      firstYear <- min(validYears)
      lastYear <- max(validYears)

      # 2. Linear interpolation on required support (midYears)
      midYears <- interpolatedYears[interpolatedYears >= firstYear & interpolatedYears <= lastYear]
      if (length(midYears) > 0) {
        interpVals <- approx(
          x = validYears,
          y = valsAll[valid],
          xout = midYears,
          method = "linear",
          rule = 1
        )$y
        out[r, midYears, v] <- interpVals
      }

      # 3. Backward projection (constant)
      backYears <- interpolatedYears[interpolatedYears < firstYear]
      if (length(backYears) > 0) {
        out[r, backYears, v] <- valsAll[valid[1]]
      }

      # 4. Forward projection (constant)
      forwardYears <- interpolatedYears[interpolatedYears > lastYear]
      if (length(forwardYears) > 0) {
        out[r, forwardYears, v] <- valsAll[valid[length(valid)]]
      }
    }
  }
  return(out)
}
