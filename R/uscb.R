#' Load data from the USCB
#' @param file path to the CSV file to load
#' @param ... currently ignored
#' @importFrom readr read_csv parse_number
#' @importFrom stringr str_sub
#' @export

load_uscb <- function(file, ...) {
  x <- readr::read_csv(file)
#  names(x) = as.character(unlist(x[1, ]))
  year <- as.numeric(stringr::str_sub(basename(file), 5, 6)) + 2000

  x <- x %>%
    filter(GEO.id != "Id")

  if (year < 2010) {
    x <- x %>%
      dplyr::select(`GEO.display-label`, HC02_EST_VC02,
                    HC02_EST_VC10,
                    HC02_EST_VC18, HC01_EST_VC18)
  } else if (year < 2013) {
    x <- x %>%
      dplyr::select(`GEO.display-label`, HC02_EST_VC02,
                    HC02_EST_VC12,
                    HC02_EST_VC22, HC01_EST_VC22)
  } else {
    x <- x %>%
      dplyr::select(`GEO.display-label`, HC02_EST_VC02,
                    HC02_EST_VC11,
                    HC02_EST_VC20, HC01_EST_VC20)
  }

  x <- x %>%
    rename(Year = year)
  names(x) <- c("State", "household_income",
                "family_income",
                "per_capita_income", "population", "Year")

  x <- x %>%
    mutate(household_income = readr::parse_number(household_income),
           family_income = readr::parse_number(family_income),
           per_capita_income = readr::parse_number(per_capita_income),
           population = readr::parse_number(population))
}
