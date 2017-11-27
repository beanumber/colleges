context("colleges")
library(tidyverse)

test_that("basketball works", {
  expect_true(
    all(
      64 < basketball %>%
        filter(tourney) %>%
        group_by(Year) %>%
        summarize(num_teams = n()) %>%
        pull(num_teams)
      )
  )
  basketball %>%
    filter(bb_champs) %>%
    group_by(Year) %>%
    summarize(num_teams = n(), champ = first(school_name))
})
