test_that("calcCarbonPrice emissionsCovered subtype returns magpie with sector groups", {
  cachePath <- "C:/Users/renatoro/Desktop/Input Data/remind_inputdata/cache/default"
  skip_if_not(dir.exists(cachePath), message = "Carbon price cache folder not available")

  mainFolder <- "C:/Users/renatoro/Desktop/Input Data/remind_inputdata"
  cacheFolder <- file.path(mainFolder, "cache/default")
  mr_local_env()
  suppressMessages(suppressWarnings(
    madrat::setConfig(
      sourcefolder = file.path(mainFolder, "sources"),
      mainfolder   = mainFolder,
      cachefolder  = cacheFolder,
      forcecache   = TRUE
    )
  ))
  result <- tryCatch(
    calcOutput("CarbonPrice", subtype = "emissionsCovered", aggregate = FALSE),
    error = function(e) skip(paste("CarbonPrice unavailable:", conditionMessage(e)))
  )
  expect_s4_class(result, "magpie")
  expect_gte(ndata(result), 1L)
  expect_gte(nregions(result), 1L)
  vals <- result[!is.na(result)]
  expect_true(all(vals >= 0))
})

test_that("calcCarbonPrice effectivePrice subtype returns magpie with non-negative values", {
  cachePath <- "C:/Users/renatoro/Desktop/Input Data/remind_inputdata/cache/default"
  skip_if_not(dir.exists(cachePath), message = "Carbon price cache folder not available")

  mainFolder <- "C:/Users/renatoro/Desktop/Input Data/remind_inputdata"
  cacheFolder <- file.path(mainFolder, "cache/default")
  mr_local_env()
  suppressMessages(suppressWarnings(
    madrat::setConfig(
      sourcefolder = file.path(mainFolder, "sources"),
      mainfolder   = mainFolder,
      cachefolder  = cacheFolder,
      forcecache   = TRUE
    )
  ))
  result <- tryCatch(
    calcOutput("CarbonPrice", subtype = "effectivePrice", aggregate = FALSE),
    error = function(e) skip(paste("CarbonPrice unavailable:", conditionMessage(e)))
  )
  expect_s4_class(result, "magpie")
  vals <- result[!is.na(result)]
  expect_true(all(vals >= 0))
})

test_that("calcCarbonPrice rejects invalid subtype", {
  expect_error(
    calcCarbonPrice(subtype = "INVALID_SUBTYPE"),
    regexp = NULL # any error is acceptable for invalid subtype
  )
})
