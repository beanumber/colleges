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
    filter(X1 != "")

  var_names <- fball_data[1, ]
  var_names[7:9] <- paste0("Conf_", var_names[7:9])
  var_names[11] <- "Pts_Allowed"
  names(fball_data) <- var_names

  fball_data %>%
    filter(School != "School") %>%
    mutate(Year = paste0(year, "-", year + 1),
           school_name = gsub("Pitt", "Pittsburgh", School),
           school_name = gsub("UCF", "Central Florida", school_name),
           school_name = gsub("California", "University of California", school_name),
           school_name = gsub("SMU", "Southern Methodist", school_name),
           school_name = gsub("UAB", "Alabama-Birmingham", school_name),
           school_name = gsub("Middle Tennessee State", "Middle Tennessee", school_name),
           school_name = gsub("UTSA", "Texas-San Antonio", school_name),
           school_name = gsub("UTEP", "Texas-El Paso", school_name),
           school_name = gsub("USC", "Southern California", school_name),
           school_name = gsub("LSU", "Louisiana State", school_name),
           school_name = gsub("Ole Miss", "Mississippi", school_name),
           fb_final_ranking = as.numeric(`AP Post`),
           fb_champs = `AP Post` == 1,
           fb_final_four = fb_final_ranking <= 4)
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
    filter(X1 != "")

  var_names <- bball_data[1, ]
  keep <- !is.na(var_names)
  var_names[7:10] <- paste0("Conf_", var_names[7:10])
  bball_data <- bball_data[, keep]
  names(bball_data) <- var_names[keep]

  bball_data %>%
    filter(School != "School") %>%
    mutate(Year = paste0(year - 1, "-", year),
           bb_final_ranking = as.numeric(`AP Final`),
           # not the tournament champion!!
           bb_champs = `AP Final` == 1,
           fb_final_four = bb_final_ranking <= 4,
           tourney = grepl(Notes, pattern = "NCAA Tournament"),
           conf_champ = grepl(Notes, pattern = "Reg. Season Champion"),
           conf_tourney = grepl(Notes, pattern = "Conf. Tournament Champion")) %>%
  rename(school_name = School)
}

#' Combine basketball and football data
#' @export

get_sports <- function() {
  sports <- basketball %>%
    full_join(football, by = c("school_name", "Year")) %>%
    mutate(bball_wins = parse_number(W.x), bball_losses = parse_number(L.x),
           fball_wins = parse_number(W.y), fball_losses = parse_number(L.y),
           bball_wpct = bball_wins / (bball_wins + bball_losses),
           fball_wpct = fball_wins / (fball_wins + fball_losses))
}

#' Combine sports and academic data
#' @export

get_academics <- function() {
  sports <- get_sports()
  sports %>%
    left_join(schools, by = c("school_name" = "sports_ref_name")) %>%
    left_join(ipeds, by = c("ipeds_name" = "school_name", "Year")) %>%
    mutate(is_military = ifelse(school_name %in% c("Air Force", "Army", "Navy"), TRUE, FALSE),
           is_private = (`State.appropriations..F` == 0),
           state_funding = `State.appropriations..F`,
           dollars_per_capita = `State.appropriations..F` / Enrolled.total,
           enrolled = Enrolled.total,
           admitted = Admissions.total,
           applied = Applicants.total,
           yield = enrolled / admitted,
           admit_rate = admitted / applied,
           act_composite_25 = ACT.Composite.25th.percentile.score,
           act_composite_75 = ACT.Composite.75th.percentile.score,
           sat_25_total = ifelse(is.na(SAT.Critical.Reading.25th.percentile.score),
                                 0, SAT.Critical.Reading.25th.percentile.score) +
             ifelse(is.na(SAT.Math.25th.percentile.score),
                    0, SAT.Math.25th.percentile.score) +
             ifelse(is.na(SAT.Writing.25th.percentile.score),
                    0, SAT.Writing.25th.percentile.score),
           sat_25_num = ifelse(is.na(SAT.Critical.Reading.25th.percentile.score),
                               0, 1) +
             ifelse(is.na(SAT.Math.25th.percentile.score),
                    0, 1) +
             ifelse(is.na(SAT.Writing.25th.percentile.score),
                    0, 1),
           sat_75_total = ifelse(is.na(SAT.Critical.Reading.75th.percentile.score),
                                 0, SAT.Critical.Reading.75th.percentile.score) +
             ifelse(is.na(SAT.Math.75th.percentile.score),
                    0, SAT.Math.75th.percentile.score) +
             ifelse(is.na(SAT.Writing.75th.percentile.score),
                    0, SAT.Writing.75th.percentile.score),
           sat_75_num = ifelse(is.na(SAT.Critical.Reading.75th.percentile.score),
                               0, 1) +
             ifelse(is.na(SAT.Math.75th.percentile.score),
                    0, 1) +
             ifelse(is.na(SAT.Writing.75th.percentile.score),
                    0, 1),
           sat_25_avg = sat_25_total / sat_25_num,
           sat_75_avg = sat_75_total / sat_75_num) %>%
    select(Year, school_name, is_military, is_private, state_funding, dollars_per_capita,
           applied, admitted, enrolled, yield, admit_rate,
           act_composite_25, act_composite_75, sat_25_avg, sat_75_avg,
           bball_wins, bball_losses, bball_wpct, bb_final_ranking, bb_champs, bb_final_four, tourney, conf_champ, conf_tourney,
           fball_wins, fball_losses, fball_wpct, fb_final_ranking, fb_champs, fb_final_four)
}

