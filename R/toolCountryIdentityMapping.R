# nolint start
#' @title toolCountryIdentityMapping
#' @description Generates (once) and returns the country-identity region mapping,
#' so a country-resolution run needs no hand-made mapping file. The mapping maps
#' every country to itself (`RegionCode == CountryCode`), which makes
#' `aggregate = TRUE` a per-country no-op: the whole regional pipeline runs at
#' country resolution unchanged. The file is derived from the shipped
#' `regionmappingH12.csv` (mapping folder if present, else the copy bundled with
#' madrat) and written to `<mappingfolder>/regional/regionmapping_country.csv`
#' on first use; subsequent calls return the existing file untouched.
#'
#' Callers that pass the returned name to `calcOutput(regionmapping = )` must
#' remember that `calcPolicyStringency()`'s coverage filter reads the GLOBAL
#' `madrat::getConfig("regionmapping")` — the pfm panel builders scope the global
#' config around their data calls when given the `"country"` sentinel (see
#' `pfm::resolveRegionMapping`).
#'
#' @param fileName character; name of the generated mapping file inside the
#'   regional mapping folder.
#' @return The mapping file name (character), resolvable by
#'   [`madrat::toolGetMapping()`] with `type = "regional"` and
#'   `where = "mappingfolder"`.
#' @author Renato Rodrigues
#'
#' @importFrom madrat getConfig toolGetMapping
#' @importFrom utils read.csv2 write.table
#' @export
#'
toolCountryIdentityMapping <- function(fileName = "regionmapping_country.csv") {
  mapDir <- file.path(madrat::getConfig("mappingfolder"), "regional")
  target <- file.path(mapDir, fileName)
  if (file.exists(target)) {
    return(fileName)
  }

  h12File <- file.path(mapDir, "regionmappingH12.csv")
  m <- if (file.exists(h12File)) {
    utils::read.csv2(h12File) # X;CountryCode;RegionCode
  } else {
    madrat::toolGetMapping("regionmappingH12.csv", type = "regional", where = "madrat")
  }
  need <- c("CountryCode", "RegionCode")
  if (!all(need %in% colnames(m))) {
    stop("toolCountryIdentityMapping: source H12 mapping lacks column(s) ",
         paste(setdiff(need, colnames(m)), collapse = ", "),
         " - found: ", paste(colnames(m), collapse = ", "), call. = FALSE)
  }
  m$RegionCode <- m$CountryCode # each country is its own region

  dir.create(mapDir, showWarnings = FALSE, recursive = TRUE)
  utils::write.table(m, target, sep = ";", row.names = FALSE, quote = FALSE)
  fileName
}
# nolint end
