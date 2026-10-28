#' Read World Bank fossil rents
#'
#' Reads `wdi-fossil-rents.csv` (written by [downloadWDIRents()]): oil, natural gas and coal rents, \% of GDP.
#'
#' @return A [`magpie`][magclass::magclass] object `[iso3c, year, oil|gas|coal]`.
#' @author Renato Rodrigues
#' @importFrom utils read.csv
#' @importFrom magclass as.magpie
readWDIRents <- function() {
  f <- "wdi-fossil-rents.csv"
  if (!file.exists(f)) stop("readWDIRents: ", f, " not in the WDIRents source folder (downloadWDIRents())")
  x <- utils::read.csv(f, stringsAsFactors = FALSE)
  need <- c("iso3c", "year", "oil", "gas", "coal")
  if (!all(need %in% names(x))) stop("readWDIRents: ", f, " needs the columns ", paste(need, collapse = ", "))
  x <- x[nchar(x$iso3c) == 3 & is.finite(x$year), need]
  long <- stats::reshape(x, direction = "long", varying = c("oil", "gas", "coal"), v.names = "value",
                         timevar = "type", times = c("oil", "gas", "coal"), idvar = c("iso3c", "year"))
  magclass::as.magpie(long[, c("iso3c", "year", "type", "value")], spatial = 1, temporal = 2, datacol = 4)
}
