#' Normalize magpie object
#'
#' @description
#' Normalizes a magpie object using min-max scaling (x - min / (max - min)).
#' Normalization is performed per variable (dimension 3).
#'
#' @param data A magpie object to be normalized.
#' @param method Normalization method. Currently only "min-max" is supported.
#' @param scope Normalization scope: "global" (across all regions and years),
#' "region" (per region across years), or "time" (per year across regions).
#' @param minVal Optional exogenous minimum value(s). Can be a single numeric,
#' a named vector/list by variable name, or NULL (calculated from data).
#' @param maxVal Optional exogenous maximum value(s). Can be a single numeric,
#' a named vector/list by variable name, or NULL (calculated from data).
#' @param symmetric Boolean. If TRUE, the range is made symmetric around zero
#' by using the maximum absolute value of the bounds. Defaults to targetRange `[-1, 1]`.
#' @param targetRange Optional target range for normalization (e.g., c(0, 1) or c(-1, 1)).
#' If NULL, it defaults to `[0, 1]` if all values are non-negative, and `[-1, 1]` otherwise.
#'
#' @return A normalized magpie object.
#' @author Renato Rodrigues
#'
#' @importFrom magclass getNames getItems getYears
#'
#' @export
toolNormalize <- function(data, method = "min-max", scope = "global",
                          minVal = NULL, maxVal = NULL, symmetric = FALSE,
                          targetRange = NULL) {
  if (method != "min-max") {
    stop("Only 'min-max' method is currently supported.")
  }

  out <- data
  vars <- getNames(data)
  nVars <- if (is.null(data)) 0 else dim(data)[3]

  for (i in seq_len(nVars)) {
    v <- if (is.null(vars)) NULL else vars[i]
    vMinExo <- .getExo(minVal, v)
    vMaxExo <- .getExo(maxVal, v)

    if (scope == "global") {
      out[, , i] <- .normalizeSubset(data[, , i], vMinExo, vMaxExo, symmetric, targetRange)
    } else if (scope == "region") {
      for (r in magclass::getItems(data, dim = 1)) {
        out[r, , i] <- .normalizeSubset(data[r, , i], vMinExo, vMaxExo, symmetric, targetRange)
      }
    } else if (scope == "time") {
      for (y in getYears(data)) {
        out[, y, i] <- .normalizeSubset(data[, y, i], vMinExo, vMaxExo, symmetric, targetRange)
      }
    } else {
      stop("Unknown scope: ", scope)
    }
  }
  return(out)
}

#' Helper to get exogenous value
#' @noRd
.getExo <- function(exo, v) {
  if (is.null(exo)) {
    return(NULL)
  }
  if (length(exo) == 1 && is.null(names(exo))) {
    return(as.numeric(exo))
  }
  if (!is.null(names(exo)) && !is.null(v) && v %in% names(exo)) {
    return(as.numeric(exo[v]))
  }
  return(NULL)
}

#' Helper to normalize a subset of data
#' @noRd
.normalizeSubset <- function(x, vMinExo, vMaxExo, symmetric, targetRange) {
  vMin <- if (!is.null(vMinExo)) vMinExo else min(x, na.rm = TRUE)
  vMax <- if (!is.null(vMaxExo)) vMaxExo else max(x, na.rm = TRUE)

  if (symmetric) {
    maxAbs <- max(abs(vMin), abs(vMax), na.rm = TRUE)
    vMin <- -maxAbs
    vMax <- maxAbs
  }

  # Determine target range
  if (is.null(targetRange)) {
    if (symmetric || vMin < 0) {
      targetRange <- c(-1, 1)
    } else {
      targetRange <- c(0, 1)
    }
  }

  lowerBound <- targetRange[1]
  upperBound <- targetRange[2]

  if (is.finite(vMin) && is.finite(vMax) && vMax > vMin) {
    return(lowerBound + (x - vMin) * (upperBound - lowerBound) / (vMax - vMin))
  } else if (is.finite(vMin) && is.finite(vMax) && vMax == vMin) {
    return(x * 0) # Returns 0 with same structure
  } else {
    return(x * NA)
  }
}
