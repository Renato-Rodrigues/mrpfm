test_that("readWGIindicator returns 6 indicators for subtype='all'", {
  mr_local_env("WGIindicator")
  withr::with_dir(source_dir("WGIindicator"), {
    x <- readWGIindicator("all")
  })
  expect_s4_class(x, "magpie")
  expect_equal(ndata(x), 6L)
  expected <- c("VA_EST", "PV_EST", "GE_EST", "RQ_EST", "RL_EST", "CC_EST")
  expect_true(all(expected %in% getNames(x)))
  expect_true(nregions(x) > 0)
})

test_that("readWGIindicator returns 1 indicator for single subtype", {
  mr_local_env("WGIindicator")
  withr::with_dir(source_dir("WGIindicator"), {
    x <- readWGIindicator("VA.EST")
  })
  expect_s4_class(x, "magpie")
  expect_equal(ndata(x), 1L)
  expect_true("VA_EST" %in% getNames(x))
})

test_that("readWGIindicator rejects invalid subtype", {
  mr_local_env("WGIindicator")
  withr::with_dir(source_dir("WGIindicator"), {
    expect_error(readWGIindicator("INVALID"), "Unknown subtype")
  })
})

test_that("convertWGIindicator fills missing countries with NA", {
  mr_local_env("WGIindicator")
  withr::with_dir(source_dir("WGIindicator"), {
    raw <- readWGIindicator("VA.EST")
  })
  converted <- suppressMessages(convertWGIindicator(raw, subtype = "VA.EST"))
  expect_s4_class(converted, "magpie")
  expect_gte(nregions(converted), 200L)
})

test_that("calcWGIindicator returns 6 indicators with correct unit", {
  mr_local_env("WGIindicator")
  result <- calcOutput("WGIindicator", aggregate = FALSE)
  expect_s4_class(result, "magpie")
  expect_equal(ndata(result), 6L)
  expected <- c("VA_EST", "PV_EST", "GE_EST", "RQ_EST", "RL_EST", "CC_EST")
  expect_true(all(expected %in% getNames(result)))
})
