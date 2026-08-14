#' Read OECD Environmental Policy Stringency (EPS) data
#'
#' @description
#' Reads the EPS composite index from \code{eps.csv} in the source folder
#' (\code{iso3, year, eps}) and returns it as a magpie object.
#'
#' EPS is a discontinued OECD product covering 1990-2020, superseded by CAPMF. It is
#' kept as an independent cross-dataset check on the policy-stringency measure the PFM
#' models estimate on, not as a model input — see \code{\link{downloadOECD_EPS}}.
#'
#' @return A [`magpie`][magclass::magclass] object with dimensions
#'   `[iso3c, year, "eps"]`.
#'
#' @author Renato Rodrigues
#'
#' @importFrom utils read.csv
#' @importFrom magclass as.magpie getSets<-
#'
#' @seealso \code{\link{downloadOECD_EPS}}, \code{\link{convertOECD_EPS}}
#'
#' @export
readOECD_EPS <- function() {   # nolint: object_name_linter
  csvFile <- "eps.csv"
  if (!file.exists(csvFile)) {
    stop("readOECD_EPS: 'eps.csv' not found in the source folder. ",
         "madrat should have called downloadOECD_EPS() to install the bundled ",
         "snapshot; run madrat::downloadSource('OECD_EPS') if it did not.", call. = FALSE)
  }
  x <- utils::read.csv(csvFile, stringsAsFactors = FALSE)

  need <- c("iso3", "year", "eps")
  miss <- setdiff(need, colnames(x))
  if (length(miss)) {
    stop("readOECD_EPS: eps.csv is missing column(s): ", paste(miss, collapse = ", "),
         ". Expected ", paste(need, collapse = ", "), ".", call. = FALSE)
  }
  x <- x[!is.na(x$iso3) & nzchar(x$iso3) & !is.na(x$year), need, drop = FALSE]

  # The published index runs 0-6. A value outside that is a parsing fault, not data,
  # and silently carrying it into the cross-dataset correlation would quietly bias the
  # one number this source exists to produce.
  bad <- x$eps[is.finite(x$eps) & (x$eps < 0 | x$eps > 6)]
  if (length(bad)) {
    stop("readOECD_EPS: ", length(bad), " value(s) outside the published 0-6 range ",
         "(observed ", paste(round(range(bad), 3), collapse = " to "), ").", call. = FALSE)
  }

  out <- magclass::as.magpie(x, spatial = "iso3", temporal = "year", datacol = 3)
  magclass::getSets(out) <- c("region", "year", "variable")
  out
}
