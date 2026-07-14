test_that("toolCountryIdentityMapping generates a valid identity mapping", {
  mrLocalEnv()

  fileName <- toolCountryIdentityMapping()
  expect_identical(fileName, "regionmapping_country.csv")

  target <- file.path(madrat::getConfig("mappingfolder"), "regional", fileName)
  expect_true(file.exists(target))

  m <- utils::read.csv2(target)
  expect_true(all(c("CountryCode", "RegionCode") %in% colnames(m)))
  # identity: every country is its own region
  expect_identical(m$RegionCode, m$CountryCode)
  # full madrat country universe (249 ISO3 countries)
  expect_gte(nrow(m), 200)
  expect_true(all(nchar(m$CountryCode) == 3))

  # resolvable through the standard madrat mapping API
  viaMadrat <- madrat::toolGetMapping(fileName, type = "regional", where = "mappingfolder")
  expect_identical(nrow(viaMadrat), nrow(m))
})

test_that("toolCountryIdentityMapping is idempotent", {
  mrLocalEnv()

  f1 <- toolCountryIdentityMapping()
  target <- file.path(madrat::getConfig("mappingfolder"), "regional", f1)
  before <- file.mtime(target)

  f2 <- toolCountryIdentityMapping()
  expect_identical(f1, f2)
  expect_identical(file.mtime(target), before) # untouched on second call
})

test_that("toolCountryIdentityMapping prefers a mappingfolder H12 file when present", {
  mrLocalEnv()

  mapDir <- file.path(madrat::getConfig("mappingfolder"), "regional")
  dir.create(mapDir, showWarnings = FALSE, recursive = TRUE)
  # a tiny custom H12 source: the identity mapping must inherit its country set
  custom <- data.frame(X = c("Atlantis", "Utopia"),
                       CountryCode = c("ATL", "UTO"),
                       RegionCode = c("REG1", "REG1"))
  utils::write.table(custom, file.path(mapDir, "regionmappingH12.csv"),
                     sep = ";", row.names = FALSE, quote = FALSE)

  fileName <- toolCountryIdentityMapping()
  m <- utils::read.csv2(file.path(mapDir, fileName))
  expect_identical(m$CountryCode, c("ATL", "UTO"))
  expect_identical(m$RegionCode, c("ATL", "UTO"))
})
