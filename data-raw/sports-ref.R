# Sports-Reference

library(tidyverse)
library(colleges)

years <- 2005:2016

football <- lapply(years, get_fball_year) %>%
  bind_rows()
save(football, file = "data/football.rda", compress = "xz")

basketball <- lapply(years + 1, get_bball_year) %>%
  bind_rows()
save(basketball, file = "data/basketball.rda", compress = "xz")

