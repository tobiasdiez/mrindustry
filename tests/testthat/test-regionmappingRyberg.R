test_that("Ryberg region mapping covers every ISO country exactly once", {
  mapping <- madrat::toolGetMapping("regionmappingRyberg.csv",
                                    type = "regional", where = "mrindustry")
  countries <- madrat::getISOlist()

  expect_setequal(mapping$CountryCode, countries)
  expect_false(any(duplicated(mapping$CountryCode)))
  regionCodes <- c("NAM", "WEU", "JPN", "EECA", "OAS", "AFR", "LAM", "OCE",
                   "IND", "CHN", "MEA")
  expect_setequal(unique(mapping$RegionCode), regionCodes)
  # one name per code
  expect_equal(nrow(unique(mapping[, c("RegionName", "RegionCode")])), 11)
})
