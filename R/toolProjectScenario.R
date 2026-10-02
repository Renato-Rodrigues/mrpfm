#' @title toolProjectScenario
#' @description Projects a magpie object forward in time with configurable convergence modes.
#' Intended for institutional quality and control variables where no scenario-specific projections
#' exist. Supports constant projection, or convergence toward a cross-sectional target
#' (global percentile, mean, max, or fixed value) with linear or logistic convergence shapes.
#'
#' @param x magpie object (one or more variables)
#' @param y integer vector of target years to output
#' @param mode projection mode: `"constant"` (forward-fill last value), `"global_max"`,
#'   `"global_percentile"` (default), `"global_mean"`, or `"fixed_value"`. Target is
#'   computed cross-sectionally from all regions at the last non-NA year in `x`.
#' @param percentile numeric 0-100, percentile target; used when `mode = "global_percentile"`.
#'   Default 75.
#' @param fixedValue numeric target value; used when `mode = "fixed_value"`.
#' @param convergenceYear integer year by which convergence is complete. Default 2100.
#' @param shape convergence curve shape: `"linear"` (default) or `"logistic"`.
#' @param midpointYear integer year at which 50% of the gap is closed; logistic only.
#'   Defaults to the midpoint between the anchor year (last data year) and `convergenceYear`.
#' @param keepIfAboveTarget logical. If `TRUE` (default), regions whose value at the anchor
#'   year already exceeds the target are forward-filled at that value rather than pulled down.
#'
#' @return magpie object with the same regions and variables as `x`, spanning years `y`.
#' @author Renato Rodrigues
#'
#' @importFrom magclass getItems getYears new.magpie
#' @importFrom stats approx quantile
#' @export
#'
toolProjectScenario <- function(x,
                                y,
                                mode              = "global_percentile",
                                percentile        = 75,
                                fixedValue        = NULL,
                                convergenceYear   = 2100,
                                shape             = "linear",
                                midpointYear      = NULL,
                                keepIfAboveTarget = TRUE) {
  validModes <- c("constant", "global_max", "global_percentile", "global_mean", "fixed_value")
  if (!mode %in% validModes) {
    stop("mode must be one of: ", paste(validModes, collapse = ", "))
  }
  if (!shape %in% c("linear", "logistic")) {
    stop("shape must be 'linear' or 'logistic'")
  }
  if (mode == "fixed_value" && is.null(fixedValue)) {
    stop("fixedValue must be provided when mode = 'fixed_value'")
  }

  regs   <- getItems(x, dim = 1)
  vars   <- getItems(x, dim = 3)
  y      <- sort(as.integer(y))
  out    <- new.magpie(regs, y, vars, fill = NA) # nolint: undesirable_function_linter.
  # Filled as a plain array and written into the magpie object once at the end: element-wise
  # magpie assignment cost ~38 s of every ~99 s coupling call (profile of 2026-10-02).
  oarr   <- array(NA_real_, dim = c(length(regs), length(y), length(vars)),
                  dimnames = list(regs, as.character(y), vars))
  arr    <- as.array(x)
  yrsAll <- getYears(x, as.integer = TRUE)

  for (v in vars) {
    vArr    <- arr[, , v, drop = FALSE]
    hasData <- apply(!is.na(vArr), 2, any)
    if (!any(hasData)) next
    path <- .projectionPath(vArr, yrsAll, hasData, mode, percentile, fixedValue, convergenceYear,
                            shape, midpointYear)
    for (r in regs) {
      oarr[r, , v] <- .projectRegion(as.numeric(vArr[r, , ]), yrsAll, y, path, mode, shape,
                                     convergenceYear, keepIfAboveTarget)
    }
  }
  out[, , ] <- oarr
  return(out)
}

# One variable's anchor year, cross-sectional target and logistic parameters.
.projectionPath <- function(vArr, yrsAll, hasData, mode, percentile, fixedValue, convergenceYear,
                            shape, midpointYear) {
  anchorYear <- max(yrsAll[hasData])
  # Compute convergence target from cross-sectional distribution at anchorYear
  crossSec <- as.numeric(vArr[, yrsAll == anchorYear, ])
  target <- switch(mode,
    constant          = NA_real_,
    global_max        = max(crossSec, na.rm = TRUE),
    global_percentile = as.numeric(quantile(crossSec, probs = percentile / 100, na.rm = TRUE)),
    global_mean       = mean(crossSec, na.rm = TRUE),
    fixed_value       = as.numeric(fixedValue)
  )
  path <- list(anchorYear = anchorYear, target = target)
  if (shape == "logistic" && mode != "constant") {
    mid <- if (!is.null(midpointYear)) {
      as.integer(midpointYear)
    } else {
      as.integer(round((anchorYear + convergenceYear) / 2))
    }
    if (mid >= convergenceYear) {
      stop("midpointYear (", mid, ") must be less than convergenceYear (", convergenceYear, ")")
    }
    if (mid <= anchorYear) {
      stop("midpointYear (", mid, ") must be greater than the anchor year (", anchorYear, ")")
    }
    # k derived so the raw logistic reaches ~0.99 at convergenceYear relative to midpointYear
    k <- log(99) / (convergenceYear - mid)
    logisticAnchor <- 1 / (1 + exp(-k * (anchorYear      - mid)))
    logisticConv   <- 1 / (1 + exp(-k * (convergenceYear - mid)))
    path <- c(path, list(k = k, mid = mid, logisticAnchor = logisticAnchor,
                         denom = logisticConv - logisticAnchor))
  }
  path
}

# One region's series over y: history interpolated (held flat outside the observed years) up to
# the anchor year, then the projection. NA where there is nothing to project from.
.projectRegion <- function(rVals, yrsAll, y, path, mode, shape, convergenceYear, keepIfAboveTarget) {
  res   <- rep(NA_real_, length(y))
  valid <- which(!is.na(rVals))
  if (length(valid) == 0) return(res)
  validYears <- yrsAll[valid]
  firstYear  <- min(validYears)
  lastYear   <- max(validYears)
  anchorYear <- path$anchorYear
  target     <- path$target

  # ---- Historical / pre-anchor years: interpolate from x ----
  histI <- y <= anchorYear
  res[histI & y < firstYear] <- rVals[valid[1]]
  midI <- histI & y >= firstYear & y <= lastYear
  if (any(midI)) {
    res[midI] <- approx(validYears, rVals[valid], xout = y[midI], method = "linear", rule = 1)$y
  }
  res[histI & y > lastYear] <- rVals[valid[length(valid)]]

  # ---- Projection years (after anchorYear) ----
  projI <- y > anchorYear
  if (!any(projI)) return(res)
  # startVal: value at anchorYear for this region (computed directly from x data)
  startVal <- if (anchorYear >= firstYear && anchorYear <= lastYear) {
    approx(validYears, rVals[valid], xout = anchorYear, method = "linear", rule = 1)$y
  } else if (anchorYear > lastYear) {
    rVals[valid[length(valid)]]
  } else {
    rVals[valid[1]]
  }
  if (is.na(startVal)) return(res)
  if (mode == "constant" || (keepIfAboveTarget && startVal > target)) {
    res[projI] <- startVal
    return(res)
  }
  w <- vapply(y[projI], function(yr) {
    if (yr >= convergenceYear) {
      1.0
    } else if (shape == "linear") {
      (yr - anchorYear) / (convergenceYear - anchorYear)
    } else {
      logisticT <- 1 / (1 + exp(-path$k * (yr - path$mid)))
      max(0, min(1, (logisticT - path$logisticAnchor) / path$denom))
    }
  }, numeric(1))
  res[projI] <- startVal + w * (target - startVal)
  res
}
