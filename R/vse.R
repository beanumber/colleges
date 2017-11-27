#' Load VSE data
#' @param file path to CSV file
#' @param ... currently ignored
#' @export

load_vse <- function(file, ...) {
  out <- readr::read_csv(file)

  out <- out %>%
    mutate(year = readr::parse_number(Year),
           school_name = Institution,
           current_total = parse_number(`Total Current Operations`),
           grand_total = parse_number(`Grand Total`),
           alumni_total = parse_number(`Alumni: Total`),
           athletics_total = parse_number(`Athletics/Total: Total Amount ($)`)) %>%
    select(year, school_name, ends_with("_total"))
  # clean up variable names
  # mutate to add  year column
  # use parse_number() to fix data types
  # write regular expression to strip out place name in parentheses
}
