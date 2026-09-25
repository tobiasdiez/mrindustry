#' Convert Ryberg et al. (2019) data
#'
#' Disaggregate the regional per-capita plastic consumption from
#' [`readRyberg()`] to ISO countries. Since the data is an intensive quantity
#' (kg per capita), every country is assigned the value of the Ryberg region it
#' belongs to. The world average is dropped.
#'
#' The mapping of countries to Ryberg regions is given in
#' `regionmappingRyberg.csv`. The regions are not fully defined in the paper;
#' the mapping is based on UN sub-regions with adjustments such that the
#' regional populations match those reported in Table S1 (e.g. Mexico is part
#' of North America, China includes Hong Kong, Macao and Taiwan, and Iran and
#' Egypt are part of the Middle East).
#'
#' @md
#' @param x A [`magpie`][magclass::magclass] object returned from
#'   [`readRyberg()`].
#'
#' @return A [`magpie`][magclass::magclass] object with per-capita plastic
#'   consumption in kg/cap for all ISO countries.
#'
#' @author Tobias Diez
#'
#' @seealso [`readSource()`], [`readRyberg()`]
#'
#' @export
convertRyberg <- function(x) {
  x <- x["World average", , , invert = TRUE]

  map <- toolGetMapping("regionmappingRyberg.csv", type = "regional", where = "mrindustry")

  # intensive quantity: copy regional value to each country (no weight)
  out <- toolAggregate(x, map, from = "RegionCode", to = "CountryCode")

  return(toolCountryFill(out, fill = NA, verbosity = 2))
}
