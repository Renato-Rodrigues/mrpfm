#' Read V-Dem Governance Indicators
#'
#' Reads governance indicators from the Varieties of Democracy (V-Dem)
#' Country-Year dataset CSV file.
#'
#' @description
#' Default set (`subtype = "all"`) returns four indicators:
#' \itemize{
#'   \item Rule of Law (VDem) — v2x_rule
#'   \item Vertical Accountability (VDem) — v2x_veracc
#'   \item Horizontal Accountability (VDem) — v2x_horacc
#'   \item Diagonal Accountability (VDem) — v2x_diagacc
#' }
#' All V-Dem indicators are on a 0–1 scale (higher = better governance).
#' Any other V-Dem column can be requested by passing its code as `subtype`
#' (e.g. `"v2x_corr"`). Use [`listVDemIndicators()`] to see what columns are
#' available in the installed dataset file.
#'
#' @param subtype character; `"all"` (default) for the four default indicators,
#'   or any V-Dem column code (e.g. `"v2x_corr"`, `"v2x_polyarchy"`).
#'
#' @return A [`magpie`][magclass::magclass] object with dimensions
#'   `[iso3c, year, indicator]`. Variable names follow the
#'   `"<Concept> (VDem)"` convention (see ADR 0002).
#'
#' @details
#' Place the V-Dem Country-Year CSV (e.g. `V-Dem-CY-Core-v14_0.csv`) in the
#' `VDem` madrat source folder. Download from
#' <https://www.v-dem.net/data/the-v-dem-dataset/> or generate with
#' `write.csv(vdemdata::vdem, "vdem.csv")` from the
#' [vdemdata](https://github.com/vdeminstitute/vdemdata) R package.
#'
#' `country_text_id` values are treated as Gleditsch-Ward character codes and
#' converted to ISO 3166-1 alpha-3 via the `countrycode` package. Rows that do
#' not map (historical states, aggregates) are silently dropped.
#'
#' @author Renato Rodrigues
#'
#' @importFrom utils read.csv
#' @importFrom dplyr filter select mutate rename %>% .data all_of
#' @importFrom tidyr pivot_longer
#' @importFrom magclass as.magpie getSets<-
#' @importFrom madrat toolCountryFill
#'
readVDem <- function(subtype = "all") {
  labelMap <- c(
    "v2x_rule"    = "Rule of Law (VDem)",
    "v2x_veracc"  = "Vertical Accountability (VDem)",
    "v2x_horacc"  = "Horizontal Accountability (VDem)",
    "v2x_diagacc" = "Diagonal Accountability (VDem)"
  )
  defaultCodes <- names(labelMap)

  files <- list.files(path = ".", pattern = "\\.csv$", full.names = TRUE, ignore.case = TRUE)
  if (length(files) == 0) stop("No CSV file found in VDem source folder.")
  csvFile <- files[[1]]

  codes <- if (subtype == "all") defaultCodes else subtype

  # Validate requested codes against CSV header before reading full file
  header <- names(utils::read.csv(csvFile, nrows = 0, check.names = FALSE))
  required <- c("country_text_id", "country_name", "year")
  missingBase <- setdiff(required, header)
  if (length(missingBase) > 0) {
    stop("Required base columns not found in V-Dem CSV: ", paste(missingBase, collapse = ", "))
  }
  missingCodes <- setdiff(codes, header)
  if (length(missingCodes) > 0) {
    stop(
      "V-Dem column(s) not found in CSV: ", paste(missingCodes, collapse = ", "),
      "\nUse listVDemIndicators() to see all available indicator codes."
    )
  }

  colsNeeded <- c(required, codes)
  raw <- utils::read.csv(csvFile, stringsAsFactors = FALSE, check.names = FALSE)[, colsNeeded]

  # ISO 3166-1 alpha-3 code standardization
  raw$iso3c <- countrycode::countrycode(
    raw$country_text_id,
    origin = "iso3c", destination = "iso3c", warn = FALSE
  )

  getLabel <- function(code) {
    if (code %in% names(labelMap)) labelMap[[code]] else paste0(code, " (VDem)")
  }

  df <- raw |>
    tidyr::pivot_longer(
      cols = dplyr::all_of(codes),
      names_to = "vdemCode",
      values_to = "value"
    ) |>
    dplyr::mutate(
      variable = vapply(.data$vdemCode, getLabel, character(1L)),
      year     = as.integer(.data$year),
      value    = suppressWarnings(as.numeric(.data$value))
    ) |>
    dplyr::filter(!is.na(.data$iso3c)) |>
    dplyr::select("iso3c", "year", "variable", "value")

  out <- magclass::as.magpie(df, spatial = "iso3c", temporal = "year", datacol = "value")
  magclass::getSets(out) <- c("iso3c", "year", "variable")
  out <- madrat::toolCountryFill(out, fill = NA, verbosity = 0)
  return(out)
}
