# Sports-Reference

library(tidyverse)
library(colleges)

years <- 2002:2016

football <- lapply(years, get_fball_year) %>%
  bind_rows()
# save(football, file = "data/football.rda", compress = "xz")

basketball <- lapply(years + 1, get_bball_year) %>%
  bind_rows()
# save(basketball, file = "data/basketball.rda", compress = "xz")


#
# library(rvest)
# url <- "https://en.wikipedia.org/wiki/List_of_NCAA_Division_I_men%27s_basketball_champions"
# bb_tourney_champs <- read_html(url) %>%
#   html_node(css = "table.wikitable:nth-child(10)") %>%
#   html_table() %>%
#   rename(school_name = `Winning team`) %>%
#   select(Year, school_name) %>%
#   mutate(school_name = gsub("\\[.+\\]", "", school_name),
#          Year = paste(Year - 1, Year, sep = "-"),
#          bb_tourney_champs = TRUE)
#
# basketball <- basketball %>%
#   left_join(bb_tourney_champs, by = c("Year", "school_name"))

basketball %>%
  filter(bb_champs == TRUE) %>%
  select(Year, school_name)

## Combine

sports <- basketball %>%
  full_join(football, by = c("school_name", "Year")) %>%
  mutate(bball_wins = parse_number(W.x), bball_losses = parse_number(L.x),
         fball_wins = parse_number(W.y), fball_losses = parse_number(L.y),
         bball_wpct = bball_wins / (bball_wins + bball_losses),
         fball_wpct = fball_wins / (fball_wins + fball_losses)) %>%
  rename(bb_conf = Conf.x, fb_conf = Conf.y)

save(sports, file = "data/sports.rda", compress = "xz")



