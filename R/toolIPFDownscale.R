#' @title toolIPFDownscale
#' @description Disaggregates coarse-region REMIND variables to country level by
#' preserving each carrier's historical country share within each region.
#'
#' For each group (e.g. PE), each named carrier, and each REMIND region, the
#' function computes the mean country-level value over the last \code{nHistYears}
#' valid historical years and uses those values as fixed shares that are applied
#' uniformly to every REMIND projection year:
#'
#'   country_v_t = remind_reg_v_t * (hist_country_v / sum_j hist_country_v_j)
#'
#' The denominator (e.g. petotal) is derived as the sum of the named carrier
#' allocations plus a remainder, guaranteeing:
#'   (1) every named carrier <= denominator at country level,
#'   (2) historical carrier shares (country/region) are preserved exactly for
#'       carriers with historical data,
#'   (3) country values reaggregate back to REMIND regional targets.
#'
#' @param prior magpie object (country x years x vars) with historical country
#'   data (e.g. from \code{iamHistoricalData()}).
#' @param remind magpie object (region x years x vars) with REMIND regional
#'   targets.
#' @param groups named list of group specifications. Each element must contain:
#'   \describe{
#'     \item{vars}{character vector of component variable names}
#'     \item{denom}{character scalar, name of the denominator/total variable}
#'   }
#' @param mapping data.frame with columns \code{CountryCode} and \code{RegionCode}.
#' @param nHistYears integer. Number of most-recent valid historical years used
#'   to compute carrier shares. Default 10.
#' @param tol numeric convergence tolerance (kept for API compatibility). Default 1e-6.
#' @param maxIter integer (kept for API compatibility). Default 50.
#'
#' @return magpie object (country x REMIND-years x vars).
#' @author Renato Rodrigues
#'
#' @importFrom magclass getItems getYears getNames new.magpie
#' @export
toolIPFDownscale <- function(prior, remind, groups, mapping,
                              nHistYears = 10L, tol = 1e-6, maxIter = 50) {

  remindYears  <- getYears(remind, as.integer = TRUE)
  histYearNames <- getYears(prior)
  histYearsInt  <- getYears(prior, as.integer = TRUE)

  countries    <- mapping$CountryCode
  regionCodes  <- unique(mapping$RegionCode)
  priorCtrs    <- intersect(countries, getItems(prior, dim = 1))

  outputVars <- unique(c(
    unlist(lapply(groups, `[[`, "vars")),
    unlist(lapply(groups, `[[`, "denom"))
  ))

  out <- new.magpie(cells_and_regions = countries, years = remindYears,
                    names = outputVars, fill = 0)

  # Convert prior to plain array [country, year, var] for fast slicing
  priorArr <- as.array(prior[priorCtrs, , ])

  # ── Pre-compute per-group, per-region historical carrier shares ─────────────
  # Shares are frozen from the last nHistYears valid years to avoid
  # year-to-year variation that would cause carrier share drift in projections.
  grp_region_shares <- list()  # [group_name][[region_code]] -> list(named, denom_c)

  for (gname in names(groups)) {
    grp      <- groups[[gname]]
    denomVar <- grp$denom

    # Valid historical years: at least one country has non-zero denominator data
    denomOk  <- apply(priorArr[, , denomVar, drop = FALSE], 2L,
                      function(x) any(!is.na(x) & x > 0))
    validYrs <- histYearNames[denomOk]
    useYrs   <- tail(validYrs, nHistYears)

    if (length(useYrs) == 0L) {
      grp_region_shares[[gname]] <- list()
      next
    }

    slc  <- priorArr[, useYrs, , drop = FALSE]
    pm   <- apply(slc, c(1L, 3L), mean, na.rm = TRUE)  # [country, var]
    pm[is.nan(pm)] <- 0
    pm   <- pmax(pm, 0)

    grp_region_shares[[gname]] <- list()

    for (r in regionCodes) {
      allCtrs <- mapping$CountryCode[mapping$RegionCode == r]
      ctrs    <- intersect(allCtrs, priorCtrs)
      if (length(ctrs) == 0L) next

      hist_denom_c <- pm[ctrs, denomVar]
      denom_sum    <- sum(hist_denom_c)

      # Per-carrier shares: country fraction of regional total for that carrier
      namedVars <- grp$vars
      carrier_shares <- matrix(0, nrow = length(ctrs), ncol = length(namedVars),
                               dimnames = list(ctrs, namedVars))
      for (v in namedVars) {
        hist_v <- pm[ctrs, v]
        hs     <- sum(hist_v)
        if (hs > 0) {
          carrier_shares[, v] <- hist_v / hs
        } else {
          # Carrier absent in history: fall back to denominator shares
          carrier_shares[, v] <- if (denom_sum > 0) hist_denom_c / denom_sum
                                  else rep(1 / length(ctrs), length(ctrs))
        }
      }

      # Remainder shares (denom portion not covered by named carriers)
      hist_named_sum   <- rowSums(pm[ctrs, namedVars, drop = FALSE])
      hist_remainder_c <- pmax(0, hist_denom_c - hist_named_sum)
      rem_sum          <- sum(hist_remainder_c)
      remainder_shares <- if (rem_sum > 0) hist_remainder_c / rem_sum
                          else if (denom_sum > 0) hist_denom_c / denom_sum
                          else rep(1 / length(ctrs), length(ctrs))

      grp_region_shares[[gname]][[r]] <- list(
        ctrs             = ctrs,
        carrier_shares   = carrier_shares,
        remainder_shares = remainder_shares,
        namedVars        = namedVars,
        denomVar         = denomVar
      )
    }
  }

  # ── Apply shares to every REMIND year ───────────────────────────────────────
  for (t in remindYears) {
    for (gname in names(groups)) {
      grp_shares <- grp_region_shares[[gname]]
      if (length(grp_shares) == 0L) next

      grp      <- groups[[gname]]
      namedVars <- grp$vars
      denomVar  <- grp$denom

      for (r in regionCodes) {
        rs <- grp_shares[[r]]
        if (is.null(rs)) next

        ctrs <- rs$ctrs
        reg_denom <- as.numeric(remind[r, t, denomVar])
        if (reg_denom <= 0) next

        # Distribute each carrier by its pre-computed country shares
        named_alloc <- matrix(0, nrow = length(ctrs), ncol = length(namedVars),
                              dimnames = list(ctrs, namedVars))
        for (v in namedVars) {
          reg_v <- as.numeric(remind[r, t, v])
          named_alloc[, v] <- reg_v * rs$carrier_shares[, v]
        }

        # Distribute remainder by remainder shares
        reg_named_sum <- sum(sapply(namedVars, function(v) as.numeric(remind[r, t, v])))
        reg_remainder <- max(0, reg_denom - reg_named_sum)
        remainder_alloc <- reg_remainder * rs$remainder_shares

        denom_alloc <- rowSums(named_alloc) + remainder_alloc

        for (v in namedVars) out[ctrs, t, v] <- named_alloc[, v]
        out[ctrs, t, denomVar] <- denom_alloc
      }
    }
  }

  # ── Verify reaggregation (should be exact by construction) ──────────────────
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
      "toolIPFDownscale: max reaggregation error %.2e exceeds 100 * tol (%.2e).",
      maxRelErr, tol
    ), call. = FALSE)
  }

  return(out)
}
