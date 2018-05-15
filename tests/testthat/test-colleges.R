context("colleges")
library(tidyverse)

test_that("basketball works", {
  expect_true(
    all(
      64 < sports %>%
        filter(tourney) %>%
        group_by(Year) %>%
        summarize(num_teams = n()) %>%
        pull(num_teams)
      )
  )
  expect_true(
    all(
      325 <= sports %>%
        group_by(Year) %>%
        summarize(num_teams = n()) %>%
        pull(num_teams)
    )
  )

  sports %>%
    filter(bb_champs) %>%
    group_by(Year) %>%
    summarize(num_teams = n(), champ = first(school_name))

  expect_true(
    all(
      128 >= sports %>%
        filter(!is.na(fball_wins)) %>%
        group_by(Year) %>%
        summarize(num_teams = n()) %>%
        pull(num_teams)
    )
  )

  # https://en.wikipedia.org/wiki/College_Football_Playoff_National_Championship
  sports %>%
    filter(fb_champs) %>%
    group_by(Year) %>%
    summarize(num_teams = n(), champ = first(school_name))

})

test_that("donations are in", {
  expect_lte(
    power5 %>%
      filter(acad_end_year > 2006, is.na(grand_total)) %>%
      group_by(school_name) %>%
      summarize(N = n()) %>%
      arrange(desc(N)) %>%
      nrow(),
    13)
})
