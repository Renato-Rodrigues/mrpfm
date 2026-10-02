library(madrat)   # nolint: undesirable_function_linter.
library(magclass) # nolint: undesirable_function_linter.
library(mrpfm)    # nolint: undesirable_function_linter.

`%||%` <- function(a, b) if (!is.null(a)) a else b # nolint: object_name_linter.

# Prime madrat's dependency graph once, quietly. The first readSource/calcOutput of a session scans
# the code of every installed madrat package (getMadratGraph -> getCode) and, in the throwaway
# mainfolder the tests use, warns "Mapping in <pkg>:::<fun> not found!" for hundreds of functions
# of OTHER packages (mrremind, mrdrivers, ...). Those warnings say nothing about mrpfm, but
# lucode2::buildLibrary fails on any test warning. The graph is cached for the session, so the
# tests themselves still report every warning they produce.
# Called again by mrLocalEnv() after each config change, which can invalidate the cached graph.
primeMadratGraph <- function() {
  pkgs <- madrat::getConfig("packages", verbose = FALSE)
  suppressMessages(suppressWarnings(try(madrat::getMadratGraph(packages = pkgs), silent = TRUE)))
  invisible(NULL)
}
primeMadratGraph()

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
  primeMadratGraph()
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
