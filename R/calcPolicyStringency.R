# nolint start
#' Calculate CAPMF-based Policy Stringency outcomes (PSM dependent variables)
#'
#' Aggregates the OECD CAPMF sector-level stringency indices into the outcome
#' variables of the Policy Stringency Model (ADR 0036): \strong{bulk} (mean of the
#' Electricity and Industry sector indices), \strong{diffuse} (mean of the Buildings
#' and Transport sector indices) and, where the module-level indices are present,
#' \strong{composite} (mean of the Sectoral / Cross-sectoral / International LEV1
#' module indices). Sector aggregation uses the OECD's own convention of simple
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
calcPolicyStringency <- function(source = "official", minCoverage = 0.8) {
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

  # CAPMF variables are named "<CLIM_ACT_POL code> (CAPMF)". The LEV2 sector-level
  # aggregates are matched by pattern: the exact code spelling can only be confirmed
  # against the deployed OECD dataflow, so the first pattern is the expected code and
  # the later ones are spelling fallbacks. The shortest hit wins, so a sector
  # aggregate is preferred over any longer instrument-level code.
  findVar <- function(patterns, label, required = TRUE) {
    for (p in patterns) {
      hits <- grep(p, vars, value = TRUE)
      if (length(hits) > 0) {
        hit <- hits[which.min(nchar(hits))]
        if (length(hits) > 1) {
          message("calcPolicyStringency: matched '", hit, "' for ", label,
                  " (other candidates: ", paste(setdiff(hits, hit), collapse = ", "), ")")
        }
        return(hit)
      }
    }
    if (required) {
      stop("calcPolicyStringency: no CAPMF variable found for ", label,
           ". Available LEV1/LEV2 codes: ",
           paste(grep("^LEV[12]_", vars, value = TRUE), collapse = ", "))
    }
    NULL
  }

  elec <- findVar(c("^LEV2_SEC_ELEC \\(CAPMF\\)$", "^LEV2[_A-Z]*ELEC"), "Electricity")
  ind <- findVar(c("^LEV2_SEC_IND \\(CAPMF\\)$", "^LEV2[_A-Z]*IND"), "Industry")
  buildings <- findVar(c("^LEV2_SEC_BUILDINGS \\(CAPMF\\)$", "^LEV2[_A-Z]*BUIL"), "Buildings")
  transport <- findVar(c("^LEV2_SEC_TRANSPORT \\(CAPMF\\)$", "^LEV2[_A-Z]*TRAN"), "Transport")
  lev1Sec <- findVar(c("^LEV1_SEC \\(CAPMF\\)$"), "Sectoral module", required = FALSE)
  lev1Cross <- findVar(c("^LEV1_CROSS_SEC \\(CAPMF\\)$", "^LEV1_CROSS"), "Cross-sectoral module", required = FALSE)
  lev1Int <- findVar(c("^LEV1_INT \\(CAPMF\\)$"), "International module", required = FALSE)

  rowMeanVars <- function(mobj, v) {
    v <- intersect(unlist(v[!vapply(v, is.null, logical(1))]), magclass::getNames(mobj))
    if (length(v) == 0) return(NULL)
    count <- magclass::dimSums(!is.na(mobj[, , v]), dim = 3)
    total <- magclass::dimSums(mobj[, , v], dim = 3, na.rm = TRUE)
    count[count == 0] <- NA
    return(total / count)
  }

  pieces <- list(
    bulk = rowMeanVars(x, list(elec, ind)),
    diffuse = rowMeanVars(x, list(buildings, transport)),
    composite = rowMeanVars(x, list(lev1Sec, lev1Cross, lev1Int))
  )
  if (is.null(pieces$composite)) {
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
# nolint end
