# nolint start
#' Calculate CAPMF-based Policy Stringency outcomes (PSM dependent variables)
#'
#' Aggregates the OECD CAPMF sector-level stringency indices into the outcome
#' variables of the Policy Stringency Model (ADR 0036): \strong{bulk} (mean of the
#' Electricity and Industry sector indices), \strong{diffuse} (mean of the Buildings
#' and Transport sector indices) and, where the module-level indices are present,
#' \strong{composite} (mean of the Sectoral / Cross-sectoral / International LEV1
#' module indices). Each sector index is itself reconstructed from the deployed
#' CAPMF LEV2 codes, which split every sector into market-based (`LEV2_SEC_*_MBI`)
#' and non-market-based (`LEV2_SEC_*_NMBI`) instruments: a sector's stringency is the
#' mean of its instrument-type children, and Bulk/Diffuse then average the relevant
#' sector indices. Sector aggregation uses the OECD's own convention of simple
#' averages over the available children.
#'
#' Unlike [`calcCAPMF()`], values are \strong{never imputed}: these series are model
#' \emph{outcomes}, and fabricated outcome values would contaminate the estimation.
#' Missing data is instead handled at aggregation time — the weight is set to zero
#' where the outcome is missing, so a region's value is the population-weighted mean
#' over its \emph{data-bearing} member countries only, and a region with no data at
#' all aggregates to `NA` (`zeroWeight = "setNA"`), never to a fabricated zero.
#'
#' @param source character; `"official"` (default) uses the published CAPMF database.
#'   `"expanded"` uses the extended file — \strong{currently a simulated stub written by
#'   [`downloadCAPMF()`] (mock values for PRY/AGO/EGY/PAK)}: sensitivity use only,
#'   never a headline fit.
#' @param minCoverage numeric, logical or NULL; threshold of GDP and Population
#'   coverage below which a whole region's countries are excluded (set `NA`).
#'   Defaults to 0.8. Set to `FALSE` or `NULL` to disable.
#' @param sectorResolution character; `"two"` (default) returns the Bulk/Diffuse/
#'   composite outcomes, `"four"` the raw Electricity/Industry/Buildings/Transport
#'   sector indices.
#' @param weighting how the sector indices are aggregated into Bulk and Diffuse
#'   (two-sector only; ignored for `sectorResolution = "four"`). `"equal"` (default)
#'   uses the OECD simple mean; `"ghg"` or `"gdp"` use GHG-/GDP-share weights à la
#'   Metta-Versmessen (2025), sourced in the data layer by [`computeSectorWeights()`].
#'   You may also pass weights directly as a **named numeric vector** over the sector
#'   short names (`c(elec=, ind=, buildings=, transport=)`, broadcast to all cells) or
#'   a **magpie** `[iso3c, year, sector]` of per-cell weights. Weights are renormalised
#'   within the non-missing members of each pairing.
#'
#' @return A list with:
#'   \describe{
#'     \item{x}{[`magpie`][magclass::magclass] object `[iso3c, year, variable]` with
#'       variables `bulk`, `diffuse` and (if resolvable) `composite`. NOTE: at country
#'       level, missing observations are encoded as value 0 with weight 0 (an
#'       aggregation device) — use `weight > 0` to identify real data, or
#'       `readSource("CAPMF")` for raw values.}
#'     \item{weight}{SSP2 population weights, zeroed where the outcome is missing.}
#'     \item{aggregationArguments}{`zeroWeight = "setNA"` so no-data regions stay `NA`.}
#'     \item{unit}{`"index (0-10)"`}
#'     \item{description}{character description}
#'   }
#'
#' @author Renato Rodrigues
#'
#' @importFrom madrat readSource calcOutput toolGetMapping getConfig
#' @importFrom magclass getYears getNames getItems dimSums mbind setNames time_interpolate
#'
#' @export
calcPolicyStringency <- function(source = "official", minCoverage = 0.8,
                                 sectorResolution = "two", weighting = "equal") {
  sectorResolution <- match.arg(sectorResolution, c("two", "four"))
  if (is.character(weighting)) {
    weighting <- match.arg(weighting, c("equal", "ghg", "gdp"))
  } else if (!(inherits(weighting, "magpie") ||
               (is.numeric(weighting) && !is.null(names(weighting))))) {
    stop("calcPolicyStringency: 'weighting' must be \"equal\"/\"ghg\"/\"gdp\", a named ",
         "numeric vector (elec/ind/buildings/transport), or a magpie of per-cell sector weights.")
  }
  if (identical(sectorResolution, "four") && !identical(weighting, "equal")) {
    warning("calcPolicyStringency: 'weighting' is ignored for sectorResolution=\"four\" ",
            "(raw sector indices are returned unweighted).")
  }
  subtype <- switch(source,
    official = "all",
    expanded = "expanded",
    stop("calcPolicyStringency: unknown source '", source, "' (use \"official\" or \"expanded\")")
  )
  if (source == "expanded") {
    warning("calcPolicyStringency: the 'expanded' CAPMF database is currently a simulated stub ",
            "(see downloadCAPMF). Use for sensitivity mechanics only, never for a real fit.")
  }
  x <- readSource("CAPMF", subtype = subtype, convert = TRUE)
  vars <- magclass::getNames(x)

  # CAPMF variables are named "<CLIM_ACT_POL code> (CAPMF)". The deployed OECD
  # dataflow splits each LEV2 sector into market-based (MBI) and non-market-based
  # (NMBI) instruments: LEV2_SEC_<E|I|B|T>_<MBI|NMBI>. A sector's stringency is the
  # OECD simple mean over its available instrument-type children; Bulk and Diffuse
  # then average the relevant sector indices (a faithful two-level hierarchy, so a
  # sector missing one instrument type still contributes its available value and is
  # weighted equally against the other sector). Patterns tolerate spelling drift and
  # fall back to the older long-form single codes (LEV2_SEC_ELEC).

  # rowMeanVars: NA-aware equal-weight mean over a set of variables in `mobj`.
  rowMeanVars <- function(mobj, v) {
    v <- intersect(unlist(v[!vapply(v, is.null, logical(1))]), magclass::getNames(mobj))
    if (length(v) == 0) return(NULL)
    count <- magclass::dimSums(!is.na(mobj[, , v]), dim = 3)
    total <- magclass::dimSums(mobj[, , v], dim = 3, na.rm = TRUE)
    count[count == 0] <- NA
    return(total / count)
  }

  # findVar: resolve a single LEV1 module code (shortest hit wins over any longer code).
  findVar <- function(patterns, label, required = TRUE) {
    for (p in patterns) {
      hits <- grep(p, vars, value = TRUE)
      if (length(hits) > 0) {
        return(hits[which.min(nchar(hits))])
      }
    }
    if (required) {
      stop("calcPolicyStringency: no CAPMF variable found for ", label,
           ". Available LEV1/LEV2 codes: ",
           paste(grep("^LEV[12]_", vars, value = TRUE), collapse = ", "))
    }
    NULL
  }

  # sectorChildren: all instrument-type children of a LEV2 sector (letter = E/I/B/T),
  # with a long-form fallback (altToken, e.g. ELEC) for the legacy single-code spelling.
  sectorChildren <- function(letter, altToken, label) {
    patterns <- c(
      paste0("^LEV2_SEC_", letter, "_N?MBI \\(CAPMF\\)$"), # E_MBI / E_NMBI form
      paste0("^LEV2_SEC_", letter, "_"),                   # any child of the sector
      paste0("^LEV2[_A-Z]*", altToken)                     # legacy LEV2_SEC_ELEC form
    )
    for (p in patterns) {
      hits <- grep(p, vars, value = TRUE)
      if (length(hits) > 0) {
        message("calcPolicyStringency: ", label, " built from ", paste(hits, collapse = " + "))
        return(hits)
      }
    }
    stop("calcPolicyStringency: no CAPMF variable found for ", label,
         ". Available LEV1/LEV2 codes: ",
         paste(grep("^LEV[12]_", vars, value = TRUE), collapse = ", "))
  }

  # Level 1: reconstruct each sector index from its instrument-type children.
  sectors <- list(
    elec      = rowMeanVars(x, sectorChildren("E", "ELEC", "Electricity")),
    ind       = rowMeanVars(x, sectorChildren("I", "IND", "Industry")),
    buildings = rowMeanVars(x, sectorChildren("B", "BUIL", "Buildings")),
    transport = rowMeanVars(x, sectorChildren("T", "TRAN", "Transport"))
  )
  sectors <- sectors[!vapply(sectors, is.null, logical(1))]
  for (nm in names(sectors)) {
    magclass::getNames(sectors[[nm]]) <- nm
  }
  sec <- do.call(magclass::mbind, unname(sectors))

  lev1Sec <- findVar(c("^LEV1_SEC \\(CAPMF\\)$"), "Sectoral module", required = FALSE)
  lev1Cross <- findVar(c("^LEV1_CROSS_SEC \\(CAPMF\\)$", "^LEV1_CROSS"), "Cross-sectoral module", required = FALSE)
  lev1Int <- findVar(c("^LEV1_INT \\(CAPMF\\)$"), "International module", required = FALSE)

  # Level 2: average the relevant sector indices into the PSM outcomes, OR emit the
  # four raw sector indices for the four-sector resolution (sectorResolution="four",
  # 2026-07-08): electricity/industry/buildings/transport, the CAPMF-native sectors
  # that REMIND's sectoral policy levers consume. The two-sector Bulk/Diffuse pairing
  # is the validated default (ADR 0036).
  if (identical(sectorResolution, "four")) {
    fourMap <- c(elec = "Electricity", ind = "Industry",
                 buildings = "Buildings", transport = "Transport")
    pieces <- list()
    for (nm in names(sectors)) {
      pieces[[fourMap[[nm]]]] <- magclass::setNames(sectors[[nm]], NULL)
    }
  } else {
    if (identical(weighting, "equal")) {
      bulk <- rowMeanVars(sec, list("elec", "ind"))
      diffuse <- rowMeanVars(sec, list("buildings", "transport"))
    } else {
      # "ghg"/"gdp" resolve to a per-cell weight magpie via the data layer; a magpie or
      # named vector is used as supplied.
      w <- if (is.character(weighting)) computeSectorWeights(weighting, ref = sec) else weighting
      bulk <- .weightedSectorMean(sec, c("elec", "ind"), w)
      diffuse <- .weightedSectorMean(sec, c("buildings", "transport"), w)
    }
    pieces <- list(
      bulk = bulk,
      diffuse = diffuse,
      composite = rowMeanVars(x, list(lev1Sec, lev1Cross, lev1Int))
    )
  }
  if (identical(sectorResolution, "two") && is.null(pieces$composite)) {
    message("calcPolicyStringency: no LEV1 module indices found; 'composite' outcome skipped.")
  }
  pieces <- pieces[!vapply(pieces, is.null, logical(1))]
  for (nm in names(pieces)) {
    magclass::getNames(pieces[[nm]]) <- nm
  }
  out <- do.call(magclass::mbind, unname(pieces))

  # Coverage filtering (region-level, GDP + population share of data-bearing countries)
  if (isTRUE(minCoverage)) {
    minCoverage <- 0.8
  }
  if (!isFALSE(minCoverage) && !is.null(minCoverage)) {
    hasData <- apply(!is.na(out), 1, any)
    out <- .filterRegionalCoverage(out, minCoverage, names(hasData)[hasData])
  }

  # SSP2 population weights (fallback to equal weights in isolated environments)
  pop <- tryCatch(
    suppressWarnings(calcOutput("Population", scenario = "SSP2", aggregate = FALSE)),
    error = function(e) NULL
  )
  weight <- out
  weight[, , ] <- 1
  if (!is.null(pop)) {
    yearsData <- getYears(out, as.integer = TRUE)
    pop <- time_interpolate(pop,
      interpolated_year = yearsData,
      integrate_interpolated_years = TRUE, extrapolation_type = "linear"
    )
    years <- intersect(getYears(out), getYears(pop))
    out <- out[, years, ]
    weight <- weight[, years, ]
    popName <- if ("SSP2" %in% getNames(pop)) "SSP2" else getNames(pop)[[1]]
    weight[, , getNames(out)] <- pop[, years, popName]
  }

  # Coverage-weighted aggregation without imputation: zero the weight where the
  # outcome is missing (a region mean then runs over its data-bearing countries
  # only) and let a zero weight-sum yield NA rather than a fabricated zero.
  weight[is.na(out)] <- 0
  out[is.na(out)] <- 0

  return(list(
    x = out,
    weight = weight,
    aggregationArguments = list(zeroWeight = "setNA"),
    unit = "index (0-10)",
    description = "CAPMF-based Policy Stringency Model outcomes (bulk = Electricity+Industry, diffuse = Buildings+Transport, composite = module mean); never imputed"
  ))
}

# Exclude (set NA) all countries of regions whose data-bearing members cover less
# than `minCoverage` of the region's GDP AND population (reference year 2020, or the
# latest common GDP/population year before it).
.filterRegionalCoverage <- function(x, minCoverage, coveredCountries) {
  mappingFile <- madrat::getConfig("regionmapping")
  if (is.null(mappingFile) || mappingFile == "") {
    mappingFile <- "regionmapping_54.csv"
  }
  m <- tryCatch(
    toolGetMapping(mappingFile, type = "regional", where = "mappingfolder"),
    error = function(e) NULL
  )
  if (is.null(m)) {
    warning("calcPolicyStringency: could not load ", mappingFile, ". Skipping coverage filtering.")
    return(x)
  }
  colnames(m)[2] <- "CountryCode"
  colnames(m)[3] <- "RegionCode"
  m$CountryCode <- trimws(m$CountryCode)
  m$RegionCode <- trimws(m$RegionCode)

  gdp <- tryCatch(calcOutput("GDPPast", aggregate = FALSE), error = function(e) NULL)
  pop <- tryCatch(calcOutput("PopulationPast", aggregate = FALSE), error = function(e) NULL)
  if (is.null(gdp) || is.null(pop)) {
    warning("calcPolicyStringency: GDP/Population reference unavailable. Skipping coverage filtering.")
    return(x)
  }

  years <- intersect(getYears(gdp, as.integer = TRUE), getYears(pop, as.integer = TRUE))
  targetYear <- if (2020 %in% years) 2020 else max(years)

  cc <- intersect(m$CountryCode, intersect(getItems(gdp, dim = 1), getItems(pop, dim = 1)))
  m <- m[m$CountryCode %in% cc, , drop = FALSE]
  gdpV <- as.numeric(gdp[m$CountryCode, targetYear, 1])
  popV <- as.numeric(pop[m$CountryCode, targetYear, 1])
  isCovered <- m$CountryCode %in% coveredCountries

  regionRows <- split(seq_len(nrow(m)), m$RegionCode)
  insufficient <- vapply(regionRows, function(i) {
    fullGdp <- sum(gdpV[i], na.rm = TRUE)
    fullPop <- sum(popV[i], na.rm = TRUE)
    covGdp <- sum(gdpV[i][isCovered[i]], na.rm = TRUE)
    covPop <- sum(popV[i][isCovered[i]], na.rm = TRUE)
    gdpShare <- if (fullGdp > 0) covGdp / fullGdp else 0
    popShare <- if (fullPop > 0) covPop / fullPop else 0
    gdpShare < minCoverage || popShare < minCoverage
  }, logical(1))

  if (any(insufficient)) {
    badRegions <- names(insufficient)[insufficient]
    excluded <- intersect(m$CountryCode[m$RegionCode %in% badRegions], getItems(x, dim = 1))
    x[excluded, , ] <- NA
    message("calcPolicyStringency: excluded ", length(badRegions),
            " regions with coverage below ", round(minCoverage * 100, 1), "%: ",
            paste(badRegions, collapse = ", "))
  }
  return(x)
}

# Weighted, NA-aware mean of member sector indices (e.g. c("elec","ind")) using
# per-cell weights `w`: a magpie [iso3c, year, memberSectors], or a named numeric
# vector over the sector short names (broadcast to all cells). Weights are
# renormalised within the members that are non-NA in each cell, so a cell missing one
# sector still returns the other sector's value (matching the equal-weight rule).
.weightedSectorMean <- function(sec, members, w) {
  members <- intersect(members, magclass::getNames(sec))
  if (length(members) == 0) return(NULL)
  s <- sec[, , members]
  wm <- s
  wm[, , ] <- NA
  if (inherits(w, "magpie")) {
    common <- intersect(members, magclass::getNames(w))
    if (length(common) == 0) {
      stop("calcPolicyStringency: weight magpie has none of the sectors ",
           paste(members, collapse = ", "), " in dim 3.")
    }
    yrs <- intersect(magclass::getYears(s), magclass::getYears(w))
    reg <- intersect(magclass::getItems(s, dim = 1), magclass::getItems(w, dim = 1))
    for (m in common) {
      wm[reg, yrs, m] <- w[reg, yrs, m]
    }
  } else {
    for (m in members) {
      if (!m %in% names(w)) next
      wm[, , m] <- as.numeric(w[[m]])
    }
  }
  wm[is.na(s)] <- NA
  num <- magclass::dimSums(s * wm, dim = 3, na.rm = TRUE)
  den <- magclass::dimSums(wm, dim = 3, na.rm = TRUE)
  den[den == 0] <- NA
  num / den
}
# nolint end
