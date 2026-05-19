#' List Available V-Dem Indicator Codes
#'
#' Reads the header of the V-Dem CSV file in the madrat source folder and
#' returns all indicator column codes (columns whose name begins with `"v2"`).
#'
#' @return A sorted character vector of V-Dem indicator codes. Pass any element
#'   to `readSource("VDem", subtype = <code>)` or
#'   `calcOutput("VDem", subtype = <code>)` to retrieve that indicator.
#'
#' @details
#' The four default indicators loaded by [`calcVDem()`] are:
#' `v2x_rule`, `v2x_veracc`, `v2x_horacc`, `v2x_diagacc`.
#'
#' Requires the V-Dem CSV to be present in the `VDem` madrat source folder.
#' The madrat source folder is set via `madrat::setConfig(sourcefolder = ...)`.
#'
#' @author Renato Rodrigues
#'
#' @importFrom utils read.csv
#' @importFrom madrat getConfig
#' @export
#'
listVDemIndicators <- function() {
  sourceDir <- file.path(madrat::getConfig("sourcefolder"), "VDem")
  files <- list.files(sourceDir, pattern = "\\.csv$", full.names = TRUE, ignore.case = TRUE)
  if (length(files) == 0) {
    stop(
      "No V-Dem CSV file found in: ", sourceDir,
      "\nDownload from https://www.v-dem.net/data/the-v-dem-dataset/ ",
      "and place it in the VDem source folder."
    )
  }
  header <- names(utils::read.csv(files[[1]], nrows = 0, check.names = FALSE))
  sort(header[grepl("^v2", header)])
}
