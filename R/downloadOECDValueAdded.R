# nolint start
#' Download OECD value added by economic activity (annual national accounts)
#'
#' Downloads gross value added (B1G) by ISIC rev.4 economic activity from the OECD
#' SDMX API (annual national accounts, Table 6). Used by
#' [`computeSectorWeights()`] (`kind = "gdp"`) to build the Metta-Versmessen (2025)
#' GDP-share CAPMF sector weights: D35/electricity -> elec, C/manufacturing -> ind,
#' L/real estate -> buildings, H/transport -> transport.
#'
#' NOTE (same discipline as `downloadCAPMF`): the SDMX dataflow/version in the URL
#' below may need adjustment when the OECD revises the data explorer. On any
#' download failure an existing local `oecd_value_added.csv` is reused; without one
#' the function stops loudly — nothing is silently substituted.
#'
#' @author Renato Rodrigues
#' @importFrom utils download.file
#' @export
downloadOECDValueAdded <- function() {
  # Annual national accounts, value added by activity (Table 6), gross value added
  # (B1G), all reference areas, all activities; CSV with labels so the reader can
  # match on human-readable activity names.
  url <- paste0(
    "https://sdmx.oecd.org/public/rest/data/OECD.SDD.NAD,DSD_NAMAIN10@DF_TABLE6,1.1/",
    "A..........?format=csvfilewithlabels&startPeriod=1990"
  )
  destfile <- "oecd_value_added.csv"

  message("downloadOECDValueAdded: downloading OECD value added by activity from ", url)
  tryCatch(
    utils::download.file(url, destfile = destfile, mode = "wb"),
    error = function(e) {
      if (file.exists(destfile)) {
        warning("downloadOECDValueAdded: download failed (", e$message,
                "); using the existing local '", destfile, "'.")
      } else {
        stop("downloadOECDValueAdded: download failed and no local '", destfile,
             "' exists: ", e$message,
             " — check/adjust the SDMX dataflow in downloadOECDValueAdded().")
      }
    }
  )
  invisible(destfile)
}
# nolint end
