# Sports-Reference

library(tidyverse)
library(colleges)

years <- 2005:2016

football <- lapply(years, get_fball_year) %>%
  bind_rows()
# save(football, file = "data/football.rda", compress = "xz")

basketball <- lapply(years + 1, get_bball_year) %>%
  bind_rows()
# save(basketball, file = "data/basketball.rda", compress = "xz")

sports <- basketball %>%
  full_join(football, by = c("school_name", "Year")) %>%
  mutate(bball_wins = parse_number(W.x), bball_losses = parse_number(L.x),
         fball_wins = parse_number(W.y), fball_losses = parse_number(L.y),
         bball_wpct = bball_wins / (bball_wins + bball_losses),
         fball_wpct = fball_wins / (fball_wins + fball_losses))

save(sports, file = "data/sports.rda", compress = "xz")
