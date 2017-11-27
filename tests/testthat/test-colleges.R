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
  expect_true(
    all(
      325 <= basketball %>%
        group_by(Year) %>%
        summarize(num_teams = n()) %>%
        pull(num_teams)
    )
  )

  basketball %>%
    filter(bb_champs) %>%
    group_by(Year) %>%
    summarize(num_teams = n(), champ = first(school_name))

  expect_true(
    all(
      128 >= football %>%
        group_by(Year) %>%
        summarize(num_teams = n()) %>%
        pull(num_teams)
    )
  )

  # https://en.wikipedia.org/wiki/College_Football_Playoff_National_Championship
  football %>%
    filter(fb_champs) %>%
    group_by(Year) %>%
    summarize(num_teams = n(), champ = first(school_name))

  ncaa <- get_academics() %>%
    group_by(Year) %>%
    summarize(N = n(),
              num_bball_teams = sum(!is.na(bball_wins)),
              num_fball_teams = sum(!is.na(fball_wins)),
              num_bball_no_fball = sum(!is.na(bball_wins) & is.na(fball_wins)),
              num_fball_no_bball = sum(is.na(bball_wins) & !is.na(fball_wins)),
              bball_wins = sum(bball_wins, na.rm = TRUE), bball_losses = sum(bball_losses, na.rm = TRUE),
              fball_wins = sum(fball_wins, na.rm = TRUE), fball_losses = sum(fball_losses, na.rm = TRUE),
              bball_champs = sum(bb_champs, na.rm = TRUE), fball_champs = sum(fb_champs, na.rm = TRUE),
              bb_champs = max(ifelse(bb_champs, school_name, "")),
              fb_champs = max(ifelse(fb_champs, school_name, ""), na.rm = TRUE),
              enrolled = sum(enrolled, na.rm = TRUE),
              admitted = sum(admitted, na.rm = TRUE),
              applied = sum(applied, na.rm = TRUE),
              act_composite_75 = mean(act_composite_75, na.rm = TRUE),
              sat_75 = mean(sat_75_avg, na.rm = TRUE)) %>%
    mutate(admit_rate = admitted / applied,
           yield = enrolled / admitted) %>%
    print.data.frame()

})
