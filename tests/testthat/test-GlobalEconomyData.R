test_that("readGlobalEconomyData returns a magpie object with correct dimensions", {
  mr_local_env("GlobalEconomyData")
  withr::with_dir(source_dir("GlobalEconomyData"), {
    x <- readGlobalEconomyData()
  })
  expect_s4_class(x, "magpie")
  # Should have at least 1 region, 1 year, 1 variable
  expect_gte(nregions(x), 1L)
  expect_gte(nyears(x), 1L)
  expect_gte(ndata(x), 1L)
  # Regions should be 3-char ISO codes
  expect_true(all(nchar(getRegions(x)) == 3L))
})

test_that("convertGlobalEconomyData fills years and countries", {
  mr_local_env("GlobalEconomyData")
  withr::with_dir(source_dir("GlobalEconomyData"), {
    raw <- readGlobalEconomyData()
  })
  converted <- convertGlobalEconomyData(raw)
  expect_s4_class(converted, "magpie")
  expect_gte(nregions(converted), nregions(raw))
})
