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
#' by using the maximum absolute value of the bounds. Defaults to targetRange [-1, 1].
#' @param targetRange Optional target range for normalization (e.g., c(0, 1) or c(-1, 1)).
#' If NULL, it defaults to [0, 1] if all values are non-negative, and [-1, 1] otherwise.
#'
#' @return A normalized magpie object.
#' @author Renato Rodrigues
#'
#' @importFrom magclass getNames getRegions getYears
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

  # Helper to get exogenous value
  getExo <- function(exo, v) {
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

  for (i in seq_len(nVars)) {
    v <- if (is.null(vars)) NULL else vars[i]
    vMinExo <- getExo(minVal, v)
    vMaxExo <- getExo(maxVal, v)

    if (scope == "global") {
      vMin <- if (!is.null(vMinExo)) vMinExo else min(data[, , i], na.rm = TRUE)
      vMax <- if (!is.null(vMaxExo)) vMaxExo else max(data[, , i], na.rm = TRUE)

      if (symmetric) {
        maxAbs <- max(abs(vMin), abs(vMax), na.rm = TRUE)
        vMin <- -maxAbs
        vMax <- maxAbs
      }

      # Determine target range
      vTargetRange <- targetRange
      if (is.null(vTargetRange)) {
        if (symmetric || vMin < 0) {
          vTargetRange <- c(-1, 1)
        } else {
          vTargetRange <- c(0, 1)
        }
      }

      L <- vTargetRange[1]
      U <- vTargetRange[2]

      if (is.finite(vMin) && is.finite(vMax) && vMax > vMin) {
        out[, , i] <- L + (data[, , i] - vMin) * (U - L) / (vMax - vMin)
      } else if (is.finite(vMin) && is.finite(vMax) && vMax == vMin) {
        out[, , i] <- 0
      } else {
        out[, , i] <- NA
      }
    } else if (scope == "region") {
      for (r in getRegions(data)) {
        vMin <- if (!is.null(vMinExo)) vMinExo else min(data[r, , i], na.rm = TRUE)
        vMax <- if (!is.null(vMaxExo)) vMaxExo else max(data[r, , i], na.rm = TRUE)

        if (symmetric) {
          maxAbs <- max(abs(vMin), abs(vMax), na.rm = TRUE)
          vMin <- -maxAbs
          vMax <- maxAbs
        }

        vTargetRange <- targetRange
        if (is.null(vTargetRange)) {
          if (symmetric || vMin < 0) {
            vTargetRange <- c(-1, 1)
          } else {
            vTargetRange <- c(0, 1)
          }
        }

        L <- vTargetRange[1]
        U <- vTargetRange[2]

        if (is.finite(vMin) && is.finite(vMax) && vMax > vMin) {
          out[r, , i] <- L + (data[r, , i] - vMin) * (U - L) / (vMax - vMin)
        } else if (is.finite(vMin) && is.finite(vMax) && vMax == vMin) {
          out[r, , i] <- 0
        } else {
          out[r, , i] <- NA
        }
      }
    } else if (scope == "time") {
      for (y in getYears(data)) {
        vMin <- if (!is.null(vMinExo)) vMinExo else min(data[, y, i], na.rm = TRUE)
        vMax <- if (!is.null(vMaxExo)) vMaxExo else max(data[, y, i], na.rm = TRUE)

        if (symmetric) {
          maxAbs <- max(abs(vMin), abs(vMax), na.rm = TRUE)
          vMin <- -maxAbs
          vMax <- maxAbs
        }

        vTargetRange <- targetRange
        if (is.null(vTargetRange)) {
          if (symmetric || vMin < 0) {
            vTargetRange <- c(-1, 1)
          } else {
            vTargetRange <- c(0, 1)
          }
        }

        L <- vTargetRange[1]
        U <- vTargetRange[2]

        if (is.finite(vMin) && is.finite(vMax) && vMax > vMin) {
          out[, y, i] <- L + (data[, y, i] - vMin) * (U - L) / (vMax - vMin)
        } else if (is.finite(vMin) && is.finite(vMax) && vMax == vMin) {
          out[, y, i] <- 0
        } else {
          out[, y, i] <- NA
        }
      }
    } else {
      stop("Unknown scope: ", scope)
    }
  }
  return(out)
}
