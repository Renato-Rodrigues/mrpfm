library(madrat)   # nolint: undesirable_function_linter.
library(magclass) # nolint: undesirable_function_linter.
library(mrpfm)    # nolint: undesirable_function_linter.

`%||%` <- function(a, b) if (!is.null(a)) a else b # nolint: object_name_linter.

#' Path to the package test data directory
testDataDir <- function() {
  testthat::test_path("testdata")
}

#' Path to a specific source type's test data folder
sourceDir <- function(src) {
  file.path(testDataDir(), src)
}

#' Set up an isolated madrat environment for the duration of one test.
mrLocalEnv <- function(src = NULL, env = parent.frame()) {
  oldConfig <- tryCatch(madrat::getConfig(), error = function(e) list())
  tmp <- withr::local_tempdir(.local_envir = env)
  sf <- if (!is.null(src)) testDataDir() else tmp
  suppressMessages(suppressWarnings(
    madrat::setConfig(sourcefolder = sf, mainfolder = tmp, verbosity = 0)
  ))
  withr::defer(
    suppressMessages(suppressWarnings(
      madrat::setConfig(
        sourcefolder = oldConfig$sourcefolder,
        mainfolder   = oldConfig$mainfolder,
        verbosity    = oldConfig$verbosity %||% 1 # nolint: object_usage_linter.
      )
    )),
    envir = env
  )
  invisible(tmp)
}
