#' Convert Ryberg et al. 2019 data
#'
#' Disaggregates the regional data of Ryberg et al. (2019) to ISO country
#' level using the mapping `regionmappingRyberg.csv`. Per-capita values are
#' intensive quantities, so every country receives the value of the region it
#' belongs to.
#'
#' @md
#' @param x A [`magpie`][magclass::magclass] object returned by
#'   [`readRyberg()`].
#' @param subtype One of:
#'   - `plastic_consumption_per_capita`: Per-capita plastic consumption in
#'     kg/cap.
#'
#' @return A [`magpie`][magclass::magclass] object on ISO country level.
#'
#' @author Tobias Diez
#'
#' @seealso [`readSource()`], [`readRyberg()`]
#'
#' @export
convertRyberg <- function(x, subtype = "plastic_consumption_per_capita") {
  if (subtype != "plastic_consumption_per_capita") {
    stop("Invalid subtype -- supported subtypes are: plastic_consumption_per_capita")
  }

  mapping <- toolGetMapping("regionmappingRyberg.csv", type = "regional",
                            where = "mrindustry")

  missingRegions <- setdiff(getItems(x, dim = 1), mapping$RegionCode)
  if (length(missingRegions) > 0) {
    stop("Regions without mapping to countries: ",
         paste(missingRegions, collapse = ", "))
  }

  # no weight: every country receives the per-capita value of its region
  x <- toolAggregate(x, rel = mapping, from = "RegionCode", to = "CountryCode")
  x <- toolCountryFill(x, fill = 0, verbosity = 2)

  return(x)
}
