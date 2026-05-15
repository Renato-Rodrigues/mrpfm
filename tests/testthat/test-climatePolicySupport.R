test_that("readClimatePolicySupport Vlasceanu2024 returns magpie with correct structure", {
  mr_local_env("climatePolicySupport")
  withr::with_dir(source_dir("climatePolicySupport"), {
    x <- readClimatePolicySupport("Vlasceanu2024")
  })
  expect_s4_class(x, "magpie")
  expect_equal(ndata(x), 1L)
  expect_true("Support policies climate" %in% getNames(x))
  expect_true(nregions(x) > 0)
  # values should be in percent [0, 100]
  vals <- x[!is.na(x)]
  expect_true(all(vals >= 0 & vals <= 100))
})

test_that("readClimatePolicySupport Andre2024 returns magpie with correct structure", {
  mr_local_env("climatePolicySupport")
  withr::with_dir(source_dir("climatePolicySupport"), {
    x <- readClimatePolicySupport("Andre2024")
  })
  expect_s4_class(x, "magpie")
  expect_equal(ndata(x), 1L)
  expect_true("Support political climate action" %in% getNames(x))
  vals <- x[!is.na(x)]
  expect_true(all(vals >= 0 & vals <= 100))
})

test_that("readClimatePolicySupport rejects invalid subtype", {
  mr_local_env("climatePolicySupport")
  withr::with_dir(source_dir("climatePolicySupport"), {
    expect_error(readClimatePolicySupport("invalid"), "Unknown subtype")
  })
})

test_that("convertClimatePolicySupport scales values to [0,1] and fills countries", {
  mr_local_env("climatePolicySupport")
  withr::with_dir(source_dir("climatePolicySupport"), {
    raw <- readClimatePolicySupport("Vlasceanu2024")
  })
  converted <- convertClimatePolicySupport(raw, subtype = "Vlasceanu2024")
  expect_s4_class(converted, "magpie")
  vals <- converted[!is.na(converted)]
  expect_true(all(vals >= 0 & vals <= 1))
  # should have standard ISO3c country count (~249)
  expect_gte(nregions(converted), 200L)
})

test_that("calcClimatePolicySupport returns both variables with values in [0,1]", {
  mr_local_env("climatePolicySupport")
  x <- calcOutput("ClimatePolicySupport", aggregate = FALSE)
  expect_s4_class(x, "magpie")
  expect_equal(ndata(x), 2L)
  expect_true("Support policies climate" %in% getNames(x))
  expect_true("Support political climate action" %in% getNames(x))
  vals <- x[!is.na(x)]
  expect_true(all(vals >= 0 & vals <= 1))
})
