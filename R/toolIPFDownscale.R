#' @title toolIPFDownscale
#' @description Disaggregates coarse-region REMIND variables to country level using
#' Iterative Proportional Fitting (IPF/RAS). Within each source region, IPF balances
#' a country x variable matrix against two sets of constraints simultaneously:
#' column constraints (regional totals from REMIND) and row constraints
#' (country-level totals derived by disaggregating the group denominator using
#' historical shares). The prior is the 5-year moving average of historical
#' country data centred on each REMIND year, frozen at the last 5 available
#' historical years for projection years beyond the historical range.
#'
#' This guarantees that computed shares (e.g. VRE / total electricity) are
#' always in [0, 1] at the country level, while preserving each country's
#' historical energy mix as closely as possible. Works with any REMIND region
#' mapping (e.g. H12, EU21) — the mapping is fully determined by the
#' \code{mapping} argument.
#'
#' @param prior magpie object (country x years x vars) from historical country-level
#'   data (e.g. from \code{iamHistoricalData()}).
#' @param remind magpie object (region x years x vars) with REMIND regional targets,
#'   where regions correspond to \code{RegionCode} values in \code{mapping}.
#' @param groups named list of group specifications. Each element must contain:
#'   \describe{
#'     \item{vars}{character vector of component variable names for this group}
#'     \item{denom}{character scalar, name of the group's denominator/total variable}
#'   }
#'   The remainder (denom - sum(vars)) is handled implicitly and is not in the output.
#' @param mapping data.frame with columns \code{CountryCode} and \code{RegionCode}
#'   mapping countries to source regions. Compatible with any REMIND region mapping file.
#' @param tol numeric convergence tolerance on maximum relative constraint violation.
#'   Default 1e-6.
#' @param maxIter integer maximum IPF iterations per (year, group, region). Default 50.
#'
#' @return magpie object (country x REMIND-years x vars) containing all \code{vars}
#'   and \code{denom} variables from all groups, consistent with regional totals.
#' @author Renato Rodrigues
#'
#' @importFrom magclass getItems getYears getNames new.magpie
#' @export
toolIPFDownscale <- function(prior, remind, groups, mapping, tol = 1e-6, maxIter = 50) {

  remindYears   <- getYears(remind, as.integer = TRUE)
  histYearsInt  <- getYears(prior, as.integer = TRUE)
  histYearNames <- getYears(prior)

  countries   <- mapping$CountryCode
  regionCodes <- unique(mapping$RegionCode)

  # Countries present in both the mapping and the prior
  priorCtrs <- intersect(countries, getItems(prior, dim = 1))

  outputVars <- unique(c(
    unlist(lapply(groups, `[[`, "vars")),
    unlist(lapply(groups, `[[`, "denom"))
  ))

  out <- new.magpie(cells_and_regions = countries, years = remindYears,
                    names = outputVars, fill = 0)

  # Convert prior to plain array [country, year, var] for fast slicing
  priorArr <- as.array(prior[priorCtrs, , ])

  for (t in remindYears) {

    window <- (t - 2L):(t + 2L)

    for (grp in groups) {
      namedVars <- grp$vars
      denomVar  <- grp$denom
      remCol    <- ".rem"
      allCols   <- c(namedVars, remCol)

      # Select window years where this group's denominator has actual non-zero data.
      # Each group filters independently: PE uses IEA petotal availability; SE uses
      # Ember seel; they can differ by 1-2 years at the trailing edge. This prevents
      # sparse trailing years (e.g. Ember has 2024 but IEA petotal stops at 2022)
      # from producing an all-NaN priorMean and triggering the equal-split fallback.
      denomOk  <- apply(priorArr[, , denomVar, drop = FALSE], 2L,
                        function(x) any(!is.na(x) & x > 0))
      validYrs <- histYearNames[denomOk]
      validInt <- histYearsInt[denomOk]
      winNames <- validYrs[validInt %in% window]
      if (length(winNames) == 0L) winNames <- tail(validYrs, 5L)

      slc       <- priorArr[, winNames, , drop = FALSE]
      priorMean <- apply(slc, c(1L, 3L), mean, na.rm = TRUE)  # [country, var]
      priorMean[is.nan(priorMean)] <- 0
      priorMean <- pmax(priorMean, 0)

      for (r in regionCodes) {
        allCtrs <- mapping$CountryCode[mapping$RegionCode == r]
        ctrs    <- intersect(allCtrs, priorCtrs)
        nCtrs   <- length(ctrs)

        # Regional targets from REMIND
        reg_named  <- sapply(namedVars, function(v) as.numeric(remind[r, t, v]))
        reg_denom  <- as.numeric(remind[r, t, denomVar])
        reg_remain <- max(0, reg_denom - sum(reg_named, na.rm = TRUE))
        reg_all    <- c(reg_named, setNames(reg_remain, remCol))

        if (reg_denom <= 0 || nCtrs == 0L) next

        # Row constraints: distribute regional denom by historical country shares
        hist_denom_c <- pmax(0, priorMean[ctrs, denomVar])
        sum_denom    <- sum(hist_denom_c)
        denom_c      <- if (sum_denom > 0) {
          setNames(reg_denom * hist_denom_c / sum_denom, ctrs)
        } else {
          setNames(rep(reg_denom / nCtrs, nCtrs), ctrs)
        }

        # Prior matrix P: rows = countries, cols = named vars + implicit remainder
        P <- matrix(0, nrow = nCtrs, ncol = length(allCols),
                    dimnames = list(ctrs, allCols))
        for (v in namedVars) {
          P[, v] <- priorMean[ctrs, v]
        }
        P[P < 0] <- 0
        # Remainder = denom - sum(named), clipped to 0 (avoids negative-source artefacts)
        P[, remCol] <- pmax(0, hist_denom_c - rowSums(P[, namedVars, drop = FALSE]))

        # Zero fallback: if a column is all-zero but regional target > 0, seed with denom shares
        for (v in allCols) {
          if (sum(P[, v]) == 0 && reg_all[[v]] > 0) {
            P[, v] <- denom_c
          }
        }

        # IPF: alternate column scaling (→ regional targets) and row scaling (→ country denom)
        for (iter in seq_len(maxIter)) {
          cs        <- colSums(P)
          col_scale <- ifelse(cs > 0, reg_all / cs, 0)
          P         <- sweep(P, 2L, col_scale, `*`)

          rs        <- rowSums(P)
          row_scale <- ifelse(rs > 0, denom_c / rs, 0)
          P         <- sweep(P, 1L, row_scale, `*`)

          col_err <- max(abs(colSums(P) - reg_all) / pmax(abs(reg_all), 1e-12))
          row_err <- max(abs(rowSums(P) - denom_c) / pmax(abs(denom_c), 1e-12))
          if (max(col_err, row_err) < tol) break
        }

        # Final column scaling: guarantees column constraints are exact regardless of
        # whether IPF fully converged. Needed when near-zero REMIND targets (e.g. fossil
        # phase-out) cause IPF to stall — large row-scale factors for historically
        # fossil-heavy countries repeatedly re-inflate near-zero columns.
        # Any residual row-constraint violation is absorbed by the implicit remainder.
        cs        <- colSums(P)
        col_scale <- ifelse(cs > 0, reg_all / cs, 0)
        P         <- sweep(P, 2L, col_scale, `*`)

        # Write component vars and denom to output (remainder is implicit, not stored)
        for (v in namedVars) out[ctrs, t, v] <- P[, v]
        out[ctrs, t, denomVar] <- denom_c
      }
    }
  }

  # Post-hoc: verify that country outputs reaggregate back to REMIND targets.
  # By construction this should always hold (IPF column constraint), but warn if
  # convergence failed or NaN propagated.
  outArr    <- as.array(out)
  maxRelErr <- 0
  for (t in remindYears) {
    tName <- paste0("y", t)
    for (v in outputVars) {
      for (r in regionCodes) {
        ctrsR  <- mapping$CountryCode[mapping$RegionCode == r]
        regSum <- sum(outArr[ctrsR, tName, v], na.rm = TRUE)
        regRef <- as.numeric(remind[r, t, v])
        if (abs(regRef) > 1e-12) {
          maxRelErr <- max(maxRelErr, abs(regSum - regRef) / abs(regRef))
        }
      }
    }
  }
  if (maxRelErr > 100 * tol) {
    warning(sprintf(
      "toolIPFDownscale: max reaggregation error %.2e exceeds 100 * tol (%.2e). IPF may not have converged.",
      maxRelErr, tol
    ), call. = FALSE)
  }

  return(out)
}
