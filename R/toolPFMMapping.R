#' Resolve a region mapping, preferring the configured mappingfolder
#'
#' @description
#' A thin wrapper over \code{\link[madrat]{toolGetMapping}} with an explicit,
#' documented search order and a bundled fallback, so the PFM stack runs on a machine
#' that has no REMIND input-data folder configured.
#'
#' Order:
#' \enumerate{
#'   \item the madrat \code{mappingfolder} (\code{<mappingfolder>/<type>/<name>}) —
#'     the REMIND input data, authoritative when present
#'   \item \pkg{mrpfm}'s own \code{inst/extdata/<type>/<name>}
#' }
#'
#' The mappings live here, in the data layer, rather than in \pkg{pfm}: \pkg{pfm}
#' depends on \pkg{mrpfm}, so this is the one place both can reach, and one place is
#' the point — a second copy is a second thing to drift.
#'
#' Why not simply let madrat search everything: with \code{where = NULL} madrat also
#' consults every package in \code{getConfig("packages")}, and those copies are not
#' interchangeable. madrat's own bundled \code{regionmappingH12.csv} assigns 15
#' countries to different regions than the REMIND input data — Ukraine, Georgia and
#' Moldova move between REF and NEU, Mongolia and North Korea between OAS and REF,
#' Greenland between NEU and EUR, and four Sahel states between SSA and MEA. Silently
#' picking that copy re-aggregates every regional result under a different definition
#' of the regions. So the search order here is closed, not open.
#'
#' The bundled copies are byte- and content-verified against the REMIND input data;
#' see \code{inst/extdata/PROVENANCE.md}. The fallback is a convenience for a machine
#' without the input data, not a second opinion.
#'
#' @param name Mapping file name, e.g. \code{"regionmapping_21_EU11.csv"}.
#' @param type madrat mapping type; \code{"regional"} for region mappings.
#' @param verbose Logical. Report when the bundled fallback is used.
#'
#' @return The mapping data.frame.
#'
#' @author Renato Rodrigues
#'
#' @importFrom madrat toolGetMapping getConfig
#' @importFrom utils read.csv
#'
#' @export
toolPFMMapping <- function(name, type = "regional", verbose = TRUE) {
  hit <- tryCatch(
    madrat::toolGetMapping(name, type = type, where = "mappingfolder"),
    error = function(e) NULL)
  if (!is.null(hit)) return(hit)

  bundled <- system.file("extdata", if (is.null(type)) name else file.path(type, name),
                         package = "mrpfm")
  if (nzchar(bundled) && file.exists(bundled)) {
    if (isTRUE(verbose)) {
      message("[mapping] '", name, "' not in the madrat mappingfolder (",
              madrat::getConfig("mappingfolder"), ") - using the copy bundled with mrpfm.")
    }
    return(utils::read.csv(bundled, sep = ";", stringsAsFactors = FALSE))
  }

  stop("toolPFMMapping: '", name, "' was found neither in the madrat mappingfolder (",
       madrat::getConfig("mappingfolder"), ") nor bundled with mrpfm.\n",
       "  Bundled mappings: ",
       paste(list.files(system.file("extdata", "regional", package = "mrpfm")),
             collapse = ", "), call. = FALSE)
}
