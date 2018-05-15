#' Scrape football and basketball data
#' @param year Year to scrape
#' @importFrom xml2 read_html
#' @importFrom rvest html_nodes html_table
#' @import dplyr
#' @export
#'
#'
get_fball_year <- function(year) {

  fball_url <- paste0("http://www.sports-reference.com/cfb/years/", year, "-standings.html")

  fball_data <- xml2::read_html(fball_url) %>%
    rvest::html_nodes("table") %>%
    rvest::html_table(header = FALSE) %>%
    dplyr::bind_rows() %>%
    filter_(~X1 != "")

  var_names <- fball_data[1, ]
  var_names[7:9] <- paste0("Conf_", var_names[7:9])
  var_names[11] <- "Pts_Allowed"
  names(fball_data) <- var_names

  fball_data %>%
    filter_(~School != "School") %>%
    mutate_(Year = ~paste0(year, "-", year + 1),
           school_name = ~gsub("Pitt", "Pittsburgh", School),
           school_name = ~gsub("UCF", "Central Florida", school_name),
           school_name = ~gsub("California", "University of California", school_name),
           school_name = ~gsub("SMU", "Southern Methodist", school_name),
           school_name = ~gsub("UAB", "Alabama-Birmingham", school_name),
           school_name = ~gsub("Middle Tennessee State", "Middle Tennessee", school_name),
           school_name = ~gsub("UTSA", "Texas-San Antonio", school_name),
           school_name = ~gsub("UTEP", "Texas-El Paso", school_name),
           school_name = ~gsub("USC", "Southern California", school_name),
           school_name = ~gsub("LSU", "Louisiana State", school_name),
           school_name = ~gsub("Ole Miss", "Mississippi", school_name),
           fb_final_ranking = ~as.numeric(`AP Post`),
           fb_champs = ~`AP Post` == 1,
           fb_final_four = ~fb_final_ranking <= 4)
}

#' @rdname get_fball_year
#' @importFrom tibble tidy_names
#' @export
#'
get_bball_year <- function(year) {

  bball_url <- paste0("http://www.sports-reference.com/cbb/seasons/", year, "-standings.html")

  bball_data <- xml2::read_html(bball_url) %>%
    #    https://stackoverflow.com/questions/40616357/how-to-scrape-tables-inside-a-comment-tag-in-html-with-r
    rvest::html_nodes(xpath = '//comment()') %>%
    rvest::html_text() %>%
    paste(collapse = '') %>%
    xml2::read_html() %>%
    rvest::html_nodes("table") %>%
    rvest::html_table(header = FALSE) %>%
    bind_rows() %>%
    filter_(~X1 != "")

  var_names <- bball_data[1, ]
  keep <- !is.na(var_names)
  var_names[7:10] <- paste0("Conf_", var_names[7:10])
  bball_data <- bball_data[, keep]
  names(bball_data) <- var_names[keep]

  bball_data %>%
    filter_(~School != "School") %>%
    mutate_(Year = ~paste0(year - 1, "-", year),
            bb_final_ranking = ~as.numeric(`AP Final`),
           # not the tournament champion!!
            bb_champs = ~`AP Final` == 1,
            bb_final_four = ~bb_final_ranking <= 4,
            tourney = ~grepl(Notes, pattern = "NCAA Tournament"),
            conf_champ = ~grepl(Notes, pattern = "Reg. Season Champion"),
            conf_tourney = ~grepl(Notes, pattern = "Conf. Tournament Champion")) %>%
  rename_(school_name = ~School)
}

