test_that("readSSPextensions returns magpie with multiple scenarios and variables", {
  sxlsx <- source_dir("SSPextensions")
  skip_if_not(
    length(list.files(sxlsx, pattern = "\\.xlsx$")) > 0,
    message = "SSP extensions xlsx not in testdata (file too large to ship)"
  )
  mr_local_env("SSPextensions")
  withr::with_dir(sxlsx, {
    x <- readSSPextensions()
  })
  expect_s4_class(x, "magpie")
  expect_gte(ndata(x), 1L)
  expect_gte(nregions(x), 1L)
})

test_that("convertSSPextensions fills years and countries", {
  sxlsx <- source_dir("SSPextensions")
  skip_if_not(
    length(list.files(sxlsx, pattern = "\\.xlsx$")) > 0,
    message = "SSP extensions xlsx not in testdata (file too large to ship)"
  )
  mr_local_env("SSPextensions")
  withr::with_dir(sxlsx, {
    raw <- readSSPextensions()
  })
  converted <- suppressMessages(convertSSPextensions(raw))
  expect_s4_class(converted, "magpie")
  expect_gte(nregions(converted), nregions(raw))
})

test_that("calcSSPextensions via cache returns expected variables", {
  cachePath <- "C:/Users/renatoro/Desktop/Input Data/remind_inputdata/cache/default/calcSSPextensions.rds"
  skip_if_not(file.exists(cachePath), message = "SSP cache not available")

  mainFolder <- "C:/Users/renatoro/Desktop/Input Data/remind_inputdata"
  cacheFolder <- file.path(mainFolder, "cache/default")
  mr_local_env() # saves + restores config
  suppressMessages(suppressWarnings(
    madrat::setConfig(
      sourcefolder = test_data_dir(),
      mainfolder   = mainFolder,
      cachefolder  = cacheFolder,
      forcecache   = TRUE
    )
  ))
  x <- calcOutput("SSPextensions", aggregate = FALSE)
  expect_s4_class(x, "magpie")
  expect_gte(ndata(x), 1L)
  urbanVars <- grep("[Uu]rban", getNames(x), value = TRUE)
  expect_gte(length(urbanVars), 1L)
})
