# helper-setup.R
# Shared utilities for mrpfm testthat tests.
#
# Key helpers:
#   mr_local_env(src)  — sets up an isolated madrat env in a temp dir and
#                        restores the previous config when the test exits
#   test_data_dir()    — returns path to tests/testthat/testdata/
#   source_dir(src)    — path to testdata/<src>/ (source folder for read functions)

library(madrat)
library(magclass)

#' Path to the package test data directory
test_data_dir <- function() {
  testthat::test_path("testdata")
}

#' Path to a specific source type's test data
source_dir <- function(src) {
  file.path(test_data_dir(), src)
}

#' Set up an isolated madrat environment for the duration of one test.
#'
#' Saves the current madrat config, switches to a temp mainfolder and a
#' test-data sourcefolder, then uses `withr::defer()` so the original config
#' is restored when the calling test exits.
#'
#' @param src  Source type sub‑folder inside testdata/, or NULL to skip.
#' @param env  Environment in which the deferred cleanup runs (parent frame by default).
mr_local_env <- function(src = NULL, env = parent.frame()) {
  # Capture current config so we can restore it
  oldConfig <- tryCatch(
    madrat::getConfig(),
    error = function(e) list()
  )

  tmp <- withr::local_tempdir(.local_envir = env)
  sf <- if (!is.null(src)) source_dir(src) else tmp

  suppressMessages(suppressWarnings(
    madrat::setConfig(
      sourcefolder = sf,
      mainfolder   = tmp,
      verbosity    = 0
    )
  ))

  # Restore original config when test ends
  withr::defer(
    suppressMessages(suppressWarnings(
      madrat::setConfig(
        sourcefolder = oldConfig$sourcefolder,
        mainfolder   = oldConfig$mainfolder,
        verbosity    = oldConfig$verbosity %||% 1
      )
    )),
    envir = env
  )

  invisible(tmp)
}

# Null-coalescing helper (base R doesn't have %||% before R 4.4)
`%||%` <- function(a, b) if (!is.null(a)) a else b
