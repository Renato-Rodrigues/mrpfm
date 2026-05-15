#' Read World Bank Worldwide Governance Indicators
#'
#' Reads WGI indicator data from the standard World Bank WGI Excel file
#' (`wgidataset_with_sourcedata-YYYY.xlsx`). Each indicator has its own sheet
#' with long-format data (one row per country × year combination).
#'
#' @description
#' Returns one of six governance indicators or all six combined:
#' \itemize{
#'   \item Voice and Accountability
#'   \item Political Stability
#'   \item Government Effectiveness
#'   \item Regulatory Quality
#'   \item Rule of Law
#'   \item Control of Corruption
#' }
#'
#' @param subtype character; one of the six WGI indicator codes above or
#'   `"all"` (default) to return all six.
#'
#' @return A [`magpie`][magclass::magclass] object with dimensions
#'   `[iso3c, year, indicator]`.
#'
#' @author Renato Rodrigues
#'
#' @importFrom readxl read_excel excel_sheets
#' @importFrom dplyr filter select mutate rename %>% .data
#' @importFrom magclass as.magpie mbind getSets<-
#' @importFrom madrat toolCountryFill
#'
readWGIindicator <- function(subtype = "all") {
  # Map subtype codes to sheet names (short lowercase in actual WGI file)
  indicatorMap <- c(
    "Voice and Accountability" = "va",
    "Political Stability" = "pv",
    "Government Effectiveness" = "ge",
    "Regulatory Quality" = "rq",
    "Rule of Law" = "rl",
    "Control of Corruption" = "cc"
  )

  validSubtypes <- names(indicatorMap)
  if (!subtype %in% c("all", validSubtypes)) {
    stop(
      "Unknown subtype '", subtype, "'. Must be one of: ",
      paste(c("all", validSubtypes), collapse = ", ")
    )
  }

  # madrat sets the working directory to the source folder automatically
  files <- list.files(path = ".", pattern = "\\.xlsx$", full.names = TRUE)
  if (length(files) == 0) stop("No .xlsx files found in WGIindicator source folder.")
  xlsxFile <- files[[1]]

  allSheets <- readxl::excel_sheets(xlsxFile)
  subtypes <- if (subtype == "all") validSubtypes else subtype

  readSheet <- function(code) {
    sheetName <- indicatorMap[[code]]

    # Exact match first, then case-insensitive
    sheet <- if (sheetName %in% allSheets) {
      sheetName
    } else {
      matched <- allSheets[tolower(allSheets) == tolower(sheetName)]
      if (length(matched) == 0) {
        matched <- allSheets[grepl(tolower(sheetName), tolower(allSheets), fixed = TRUE)]
      }
      if (length(matched) == 0) {
        stop(
          "Cannot find sheet for indicator: ", code,
          ". Available sheets: ", paste(allSheets, collapse = ", ")
        )
      }
      matched[[1]]
    }

    # Read with skip=0 — the 2025 WGI file uses a single-row header
    raw <- suppressMessages(
      readxl::read_excel(xlsxFile, sheet = sheet, skip = 0, col_names = TRUE)
    )

    # Column patterns in 2025 WGI format:
    #  "Economy (code)"                        → iso3c
    #  "Year"                                  → year
    #  "Governance estimate (approx. ...)"     → value
    codeColPat <- "^Economy \\(code\\)$"
    yearColPat <- "^Year$"
    valColPat <- "^Governance estimate"

    codeMatches <- grep(codeColPat, names(raw), ignore.case = FALSE, value = TRUE)
    # Fallback: match any column with 'code' that isn't 'ID variable'
    if (length(codeMatches) == 0) {
      codeMatches <- grep("economy.*code|country.*code", names(raw),
        ignore.case = TRUE, value = TRUE
      )
      codeMatches <- codeMatches[!grepl("^ID variable", codeMatches)]
    }
    yearMatches <- grep(yearColPat, names(raw), ignore.case = FALSE, value = TRUE)
    valMatches <- grep(valColPat, names(raw), ignore.case = TRUE, value = TRUE)

    if (length(codeMatches) == 0) stop("Cannot find economy code column in sheet '", sheet, "'.")
    if (length(yearMatches) == 0) stop("Cannot find Year column in sheet '", sheet, "'.")
    if (length(valMatches) == 0) stop("Cannot find Governance estimate column in sheet '", sheet, "'.")

    codeCol <- codeMatches[[1]]
    yearCol <- yearMatches[[1]]
    valCol <- valMatches[[1]]

    df <- raw %>%
      dplyr::select(iso3c = !!codeCol, year = !!yearCol, value = !!valCol) %>%
      dplyr::filter(
        !is.na(.data$iso3c),
        nchar(.data$iso3c) == 3,
        !grepl("^\\d", .data$iso3c),
        !.data$iso3c %in% c("XKX", "ANT")
      ) %>%
      dplyr::mutate(
        iso3c    = ifelse(.data$iso3c == "ADO", "AND", .data$iso3c),
        year     = as.integer(.data$year),
        variable = code,
        value    = suppressWarnings(as.numeric(.data$value))
      )

    m <- magclass::as.magpie(
      dplyr::select(df, "iso3c", "year", "variable", "value"),
      spatial = "iso3c", temporal = "year", datacol = "value"
    )
    # Fill missing countries with NA so all sheets share identical spatial dim
    m <- toolCountryFill(m, fill = NA, verbosity = 0)
    return(m)
  } # end readSheet

  result <- do.call(magclass::mbind, lapply(subtypes, readSheet))
  magclass::getSets(result) <- c("iso3c", "year", "variable")
  return(result)
}
