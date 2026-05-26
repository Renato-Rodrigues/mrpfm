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
  arr    <- as.array(x)
  yrsAll <- getYears(x, as.integer = TRUE)

  for (v in vars) {
    vArr    <- arr[, , v, drop = FALSE]
    hasData <- apply(!is.na(vArr), 2, any)
    if (!any(hasData)) next
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

    # Precompute logistic parameters once per variable (avoids re-computing inside the inner loop)
    k <- denom <- L_a <- mid <- NULL
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
      k     <- log(99) / (convergenceYear - mid)
      L_a   <- 1 / (1 + exp(-k * (anchorYear      - mid)))
      L_c   <- 1 / (1 + exp(-k * (convergenceYear - mid)))
      denom <- L_c - L_a
    }

    for (r in regs) {
      rVals <- as.numeric(vArr[r, , ])
      valid <- which(!is.na(rVals))
      if (length(valid) == 0) next

      validYears <- yrsAll[valid]
      firstYear  <- min(validYears)
      lastYear   <- max(validYears)

      # ---- Historical / pre-anchor years: interpolate from x ----
      histY <- y[y <= anchorYear]
      backY <- histY[histY < firstYear]
      midY  <- histY[histY >= firstYear & histY <= lastYear]
      fwdY  <- histY[histY > lastYear]

      if (length(backY) > 0) out[r, backY, v] <- rVals[valid[1]]
      if (length(midY)  > 0) {
        out[r, midY, v] <- approx(validYears, rVals[valid], xout = midY,
                                  method = "linear", rule = 1)$y
      }
      if (length(fwdY)  > 0) out[r, fwdY, v] <- rVals[valid[length(valid)]]

      # ---- Projection years (after anchorYear) ----
      projY <- y[y > anchorYear]
      if (length(projY) == 0) next

      # startVal: value at anchorYear for this region (computed directly from x data)
      startVal <- if (anchorYear >= firstYear && anchorYear <= lastYear) {
        approx(validYears, rVals[valid], xout = anchorYear, method = "linear", rule = 1)$y
      } else if (anchorYear > lastYear) {
        rVals[valid[length(valid)]]
      } else {
        rVals[valid[1]]
      }
      if (is.na(startVal)) next

      if (mode == "constant" || (keepIfAboveTarget && startVal > target)) {
        out[r, projY, v] <- startVal
        next
      }

      for (yr in projY) {
        if (yr >= convergenceYear) {
          w <- 1.0
        } else if (shape == "linear") {
          w <- (yr - anchorYear) / (convergenceYear - anchorYear)
        } else {
          L_t <- 1 / (1 + exp(-k * (yr - mid)))
          w   <- max(0, min(1, (L_t - L_a) / denom))
        }
        out[r, yr, v] <- startVal + w * (target - startVal)
      }
    }
  }
  return(out)
}
