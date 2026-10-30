#' Read Ryberg et al. 2019 data
#'
#' Read data from the supplementary material of Ryberg, M. W., Hauschild,
#' M. Z., Wang, F., Averous-Monnery, S. & Laurent, A. (2019). Global
#' environmental losses of plastics across their value chains. Resources,
#' Conservation and Recycling 151, 104459
#' (https://doi.org/10.1016/j.resconrec.2019.104459). All data refers to the
#' year 2015.
#'
#' @md
#' @param subtype One of:
#'   - `plastic_consumption_per_capita`: Per-capita plastic consumption in
#'     kg/cap for the eleven world regions of Ryberg et al. (Table S1).
#'     Regions are labelled with the codes from
#'     `regionmappingRyberg.csv` (`NAM`, `WEU`, `JPN`, `EECA`, `OAS`, `AFR`,
#'     `LAM`, `OCE`, `IND`, `CHN`, `MEA`).
#'
#' @return A [`magpie`][magclass::magclass] object.
#'
#' @author Tobias Diez
#'
#' @seealso [`readSource()`]
#'
#' @importFrom assertr assert not_na
#' @importFrom dplyr filter left_join mutate select
#' @importFrom rlang .data is_empty
#' @importFrom tibble tribble
#' @export
readRyberg <- function(subtype = "plastic_consumption_per_capita") {
  # ---- list all available subtypes with functions doing all the work ----
  switchboard <- list(
    plastic_consumption_per_capita = function() {
      readxl::read_xlsx(path = "./Ryberg.xlsx",
                        sheet = "Table S1",
                        range = "A3:D14",
                        col_names = c("region", "value"),
                        col_types = c("text", "skip", "skip", "numeric")) %>%
        filter(.data$region != "World average") %>%
        # use short region codes, magclass cannot handle "." in region names
        left_join(
          tribble(
            ~region,                                ~code,
            "North America",                        "NAM",
            "Western Europe",                       "WEU",
            "Japan",                                "JPN",
            "Eastern Europe & Central Asia",        "EECA",
            "Asia (excl. Japan, India, and China)", "OAS",
            "Africa",                               "AFR",
            "Latin America & Caribbean",            "LAM",
            "Oceania",                              "OCE",
            "India",                                "IND",
            "China",                                "CHN",
            "Middle East",                          "MEA"),
          "region"
        ) %>%
        assert(not_na, "code", "value") %>%
        mutate(period = 2015,
               variable = "Plastic consumption per capita") %>%
        select(region = "code", "period", "variable", "value") %>%
        as.magpie(spatial = 1, temporal = 2, tidy = TRUE)
    }
  )

  # ---- check if the subtype called is available ----
  if (is_empty(intersect(subtype, names(switchboard)))) {
    stop(paste("Invalid subtype -- supported subtypes are:",
               paste(names(switchboard), collapse = ", ")))
  } else {
    # ---- load data and do whatever ----
    return(switchboard[[subtype]]())
  }
}
