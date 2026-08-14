#' Provide the OECD Environmental Policy Stringency (EPS) source data
#'
#' @description
#' madrat calls \code{download*} when a source folder is missing, so this is what
#' makes \code{readSource("OECD_EPS")} work on a fresh checkout with no manual step.
#'
#' The EPS index is a \strong{discontinued} OECD product: the composite covers
#' 1990-2020 and has been superseded by CAPMF, which is what the PFM models actually
#' estimate on. EPS is retained only as an independent cross-dataset check
#' (\code{pfm::computePSMCrossDataset}) — a second, differently-constructed measure of
#' the same latent construct, so agreement between them is evidence the index is not
#' an artefact of one team's coding choices.
#'
#' Because the series is closed, a bundled snapshot is the honest representation: there
#' is no live endpoint that would return anything different, and re-deriving it from
#' the OECD SDMX API would introduce a network dependency and a moving target for a
#' dataset that cannot move. The snapshot ships in
#' \code{inst/extdata/OECD_EPS/eps.csv} and is copied here.
#'
#' @return Invisibly \code{TRUE}. Writes \code{eps.csv} into the source folder.
#'
#' @author Renato Rodrigues
#'
#' @seealso \code{\link{readOECD_EPS}}, \code{\link{convertOECD_EPS}}
#'
#' @export
downloadOECD_EPS <- function() {   # nolint: object_name_linter
  src <- system.file("extdata", "OECD_EPS", "eps.csv", package = "mrpfm")
  if (!nzchar(src) || !file.exists(src)) {
    stop("downloadOECD_EPS: the bundled EPS snapshot is missing from mrpfm ",
         "(inst/extdata/OECD_EPS/eps.csv).", call. = FALSE)
  }
  if (!file.copy(src, "eps.csv", overwrite = TRUE)) {
    stop("downloadOECD_EPS: could not copy the bundled snapshot into the source folder.",
         call. = FALSE)
  }
  message("downloadOECD_EPS: installed the bundled EPS snapshot (OECD EPS 1990-2020, ",
          "discontinued; superseded by CAPMF).")

  # madrat records this alongside the source so provenance survives the copy.
  return(list(url = "https://www.oecd.org/en/data/datasets/environmental-policy-stringency-index.html",
              author = "OECD",
              title = "Environmental Policy Stringency Index",
              license = "OECD Terms and Conditions",
              description = paste("Composite country-level index of environmental policy",
                                  "stringency, 1990-2020. Discontinued; superseded by CAPMF.",
                                  "Bundled snapshot - the series is closed, so there is no",
                                  "live endpoint that would return anything different."),
              unit = "index (0-6)"))
}
