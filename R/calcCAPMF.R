# nolint start
#' Calculate OECD Climate Actions and Policies Measurement Framework (CAPMF) data
#'
#' Wraps [`readSource`] for CAPMF data and returns sectoral stringency indicators
#' as a single magpie object for use as a `calcOutput`.
#'
#' @param subtype character; "all" (default) or a specific sector.
#' @param minCoverage numeric or logical; threshold of GDP and Population coverage below which a region is excluded. Defaults to 0.8. Set to FALSE or NULL to disable.
#' @param includeEstimated logical; if TRUE, estimates the composite index scores for newly onboarded developing countries.
#'
#' @return A list with:
#'   \describe{
#'     \item{x}{[`magpie`][magclass::magclass] object `[iso3c, year, variable]`}
#'     \item{weight}{SSP2 population weights for spatial aggregation}
#'     \item{unit}{`"index (0-10)"`}
#'     \item{description}{character description}
#'   }
#'
#' @author Renato Rodrigues
#'
#' @importFrom madrat readSource calcOutput toolGetMapping
#' @importFrom magclass getYears getNames time_interpolate ndata getItems dimSums mbind new.magpie as.magpie
#'
#' @export
calcCAPMF <- function(subtype = "all", minCoverage = 0.8, includeEstimated = FALSE) {
  x <- readSource("CAPMF", subtype = subtype, convert = TRUE)


  magpieRowMeans <- function(mobj, vars) {
    present_vars <- intersect(vars, magclass::getNames(mobj))
    if (length(present_vars) == 0) return(NULL)
    count <- magclass::dimSums(!is.na(mobj[, , present_vars]), dim = 3)
    total <- magclass::dimSums(mobj[, , present_vars], dim = 3, na.rm = TRUE)
    count[count == 0] <- NA
    return(total / count)
  }

  fin_est <- magpieRowMeans(x, c("LEV3_BAN_CREDIT (CAPMF)", "LEV3_BAN_FF_ABROAD (CAPMF)"))
  coord_est <- magpieRowMeans(x, c("LEV3_INT_INIT (CAPMF)", "LEV3_TREATY (CAPMF)", "LEV3_PR_AV_MAR (CAPMF)"))
  rep_est <- magpieRowMeans(x, c("LEV3_GHG_ACC (CAPMF)", "LEV3_UNFCCC (CAPMF)", "LEV3_EVAL_BR (CAPMF)"))

  l2_estimates <- list(fin_est, coord_est, rep_est)
  l2_estimates <- l2_estimates[!sapply(l2_estimates, is.null)]

  if (length(l2_estimates) > 0) {
    # Bind them along dimension 3 to compute their average
    for (i in seq_along(l2_estimates)) {
      magclass::getNames(l2_estimates[[i]]) <- paste0("TEMP_L2_", i)
    }
    l2_bound <- do.call(magclass::mbind, l2_estimates)
    estimated_int <- magpieRowMeans(l2_bound, magclass::getNames(l2_bound))
    magclass::getNames(estimated_int) <- "LEV1_EST (CAPMF)"

    if (isTRUE(includeEstimated)) {
      x <- magclass::mbind(x, estimated_int)
    }
  }

  # Identify covered countries BEFORE median imputation
  has_data <- apply(!is.na(x), 1, any)
  covered_countries <- names(has_data)[has_data]

  x <- toolImputeMedians(x)

  # Coverage filtering
  if (isTRUE(minCoverage)) {
    minCoverage <- 0.8
  }

  if (!isFALSE(minCoverage) && !is.null(minCoverage)) {
    mappingFile <- madrat::getConfig("regionmapping")
    if (is.null(mappingFile) || mappingFile == "") {
      mappingFile <- "regionmapping_54.csv"
    }
    m <- tryCatch(
      toolGetMapping(mappingFile, type = "regional", where = "mappingfolder"),
      error = function(e) {
        warning("Could not load ", mappingFile, " in calcCAPMF. Skipping coverage filtering.")
        NULL
      }
    )
    if (!is.null(m)) {
      colnames(m)[2] <- "CountryCode"
      colnames(m)[3] <- "RegionCode"
      m$CountryCode <- trimws(m$CountryCode)
      m$RegionCode <- trimws(m$RegionCode)

      gdp_ref <- tryCatch(
        calcOutput("GDPPast", aggregate = FALSE),
        error = function(e) NULL
      )
      pop_ref <- tryCatch(
        calcOutput("PopulationPast", aggregate = FALSE),
        error = function(e) NULL
      )

      if (!is.null(gdp_ref) && !is.null(pop_ref)) {
        target_year <- 2020
        available_years <- intersect(getYears(gdp_ref, as.integer = TRUE), getYears(pop_ref, as.integer = TRUE))
        if (!(target_year %in% available_years)) {
          target_year <- max(available_years)
        }

        gdp_2020 <- gdp_ref[, target_year, ]
        pop_2020 <- pop_ref[, target_year, ]

        gdp_df <- as.data.frame(gdp_2020)
        pop_df <- as.data.frame(pop_2020)

        reg_col <- if ("Region" %in% colnames(gdp_df)) "Region" else colnames(gdp_df)[1]
        val_col <- if ("Value" %in% colnames(gdp_df)) "Value" else colnames(gdp_df)[4]

        colnames(gdp_df)[colnames(gdp_df) == reg_col] <- "CountryCode"
        colnames(gdp_df)[colnames(gdp_df) == val_col] <- "gdp"
        gdp_df <- gdp_df[, c("CountryCode", "gdp")]

        reg_col_pop <- if ("Region" %in% colnames(pop_df)) "Region" else colnames(pop_df)[1]
        val_col_pop <- if ("Value" %in% colnames(pop_df)) "Value" else colnames(pop_df)[4]

        colnames(pop_df)[colnames(pop_df) == reg_col_pop] <- "CountryCode"
        colnames(pop_df)[colnames(pop_df) == val_col_pop] <- "pop"
        pop_df <- pop_df[, c("CountryCode", "pop")]

        stats_df <- merge(gdp_df, pop_df, by = "CountryCode", all = TRUE)
        stats_df <- merge(stats_df, m, by = "CountryCode", all.x = TRUE)
        stats_df <- stats_df[!is.na(stats_df$RegionCode), ]

        regions <- unique(stats_df$RegionCode)
        insufficient_regions <- c()

        for (reg in regions) {
          reg_all <- stats_df[stats_df$RegionCode == reg, ]
          reg_cov <- reg_all[reg_all$CountryCode %in% covered_countries, ]

          full_gdp <- sum(reg_all$gdp, na.rm = TRUE)
          full_pop <- sum(reg_all$pop, na.rm = TRUE)

          cov_gdp <- sum(reg_cov$gdp, na.rm = TRUE)
          cov_pop <- sum(reg_cov$pop, na.rm = TRUE)

          gdp_share <- if (full_gdp > 0) (cov_gdp / full_gdp) else 0
          pop_share <- if (full_pop > 0) (cov_pop / full_pop) else 0

          if (gdp_share < minCoverage || pop_share < minCoverage) {
            insufficient_regions <- c(insufficient_regions, reg)
          }
        }

        if (length(insufficient_regions) > 0) {
          excluded_countries <- stats_df$CountryCode[stats_df$RegionCode %in% insufficient_regions]
          x[excluded_countries, , ] <- NA

          message("calcCAPMF: Excluded ", length(insufficient_regions),
                  " regions with coverage below ", round(minCoverage * 100, 1), "%: ",
                  paste(insufficient_regions, collapse = ", "))
        }
      }
    }
  }

  pop <- tryCatch(
    suppressWarnings(calcOutput("Population", scenario = "SSP2", aggregate = FALSE)),
    error = function(e) {
      NULL
    }
  )

  if (!is.null(pop)) {
    yearsData <- getYears(x, as.integer = TRUE)
    pop <- time_interpolate(pop,
      interpolated_year = yearsData,
      integrate_interpolated_years = TRUE, extrapolation_type = "linear"
    )

    years <- intersect(getYears(x), getYears(pop))
    x   <- x[, years, ]
    pop <- pop[, years, ]

    weight <- x
    weight[, , ] <- NA
    popName <- if ("SSP2" %in% getNames(pop)) "SSP2" else getNames(pop)[[1]]
    weight[, , getNames(x)] <- pop[, , popName]
  } else {
    weight <- x
    weight[, , ] <- 1
  }

  return(list(
    x           = x,
    weight      = weight,
    unit        = "index (0-10)",
    description = "OECD Climate Actions and Policies Measurement Framework (CAPMF) indicators for the PFM"
  ))
}
# nolint end
