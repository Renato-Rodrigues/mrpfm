# nolint start
test_that("readOECDValueAdded parses the SDMX CSV into [iso3c, year, activity]", {
  mrLocalEnv("OECDValueAdded")

  x <- suppressMessages(madrat::readSource("OECDValueAdded", convert = FALSE))
  expect_true(magclass::is.magpie(x))
  expect_setequal(magclass::getItems(x, dim = 1), c("DEU", "FRA"))
  expect_setequal(magclass::getYears(x, as.integer = TRUE), c(2015, 2016))
  # activity names carry code + label, no dots/commas
  nms <- magclass::getNames(x)
  expect_true(any(grepl("D35", nms)))
  expect_true(any(grepl("Manufacturing", nms)))
  expect_false(any(grepl("[.,]", nms)))
  # the non-current-price row (PRICE_BASE Y, value 999) is filtered out
  manuf <- nms[grepl("^C ", nms)]
  expect_equal(as.numeric(x["DEU", 2015, manuf]), 30)
})

test_that("computeSectorWeights kind='gdp' builds normalised value-added shares", {
  mrLocalEnv("OECDValueAdded")

  ref <- magclass::new.magpie(c("DEU", "FRA"), c(2015, 2016), "ref", fill = 1)
  w <- suppressMessages(computeSectorWeights(kind = "gdp", ref = ref))
  expect_setequal(magclass::getNames(w), c("elec", "ind", "buildings", "transport"))
  # DEU 2015: 10/30/20/40 -> .1/.3/.2/.4 (TOTAL row must not leak into any sector)
  expect_equal(as.numeric(w["DEU", 2015, "elec"]), 0.1)
  expect_equal(as.numeric(w["DEU", 2015, "ind"]), 0.3)
  expect_equal(as.numeric(w["DEU", 2015, "buildings"]), 0.2)
  expect_equal(as.numeric(w["DEU", 2015, "transport"]), 0.4)
  # shares sum to 1 per cell
  sums <- magclass::dimSums(w, dim = 3)
  expect_true(all(abs(as.numeric(sums) - 1) < 1e-8))
})
# nolint end
