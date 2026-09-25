#' Read Ryberg et al. (2019) data
#'
#' Read per-capita plastic consumption by world region for the year 2015 from
#' Table S1 of the supplementary information of Ryberg, M. W., Laurent, A.,
#' Hauschild, M. Z., Wang, F. & Averous-Monnery, S. Global environmental losses
#' of plastics across their value chains. Resources, Conservation and Recycling
#' 151, 104459 (2019). https://doi.org/10.1016/j.resconrec.2019.104459
#'
#' The source file `Ryberg.xlsx` contains the tables of the supplementary
#' information. Values are in kg of plastic per capita and year and are given
#' for eleven world regions (North America, Western Europe, Japan, Eastern
#' Europe & Central Asia, Asia (excl Japan, India, and China), Africa, Latin
#' America & Caribbean, Oceania, India, China, Middle East) plus a world
#' average.
#'
#' @md
#' @return A [`magpie`][magclass::magclass] object with per-capita plastic
#'   consumption in kg/cap by Ryberg region for the year 2015.
#'
#' @author Tobias Diez
#'
#' @seealso [`readSource()`], [`convertRyberg()`]
#'
#' @importFrom dplyr filter select mutate
#' @importFrom rlang .data
#' @export
readRyberg <- function() {
  readxl::read_xlsx("Ryberg.xlsx", sheet = "Table S1", range = "A3:H16",
                    col_names = c("region", "productionShare", "productionRef",
                                  "consumptionPerCapita", "consumptionRef",
                                  "population", "consumptionTotal",
                                  "consumptionShare"),
                    col_types = c("text", "numeric", "text", "numeric", "text",
                                  "numeric", "numeric", "numeric")) %>%
    filter(!is.na(.data$region)) %>%
    # dots are dimension separators in magclass, so drop them from region names
    mutate(region = gsub(".", "", .data$region, fixed = TRUE),
           period = 2015L,
           variable = "Plastic consumption per capita (kg/cap)") %>%
    select("region", "period", "variable", value = "consumptionPerCapita") %>%
    as.magpie(spatial = "region", temporal = "period", tidy = TRUE)
}
