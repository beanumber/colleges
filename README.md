colleges
================

[![Travis-CI Build Status](https://travis-ci.org/beanumber/colleges.svg?branch=master)](https://travis-ci.org/beanumber/colleges)

Data about colleges
-------------------

``` r
devtools::install_github("beanumber/colleges")
```

``` r
library(tidyverse)
```

    ## ── Attaching packages ──────────────────────────────────────────────────────── tidyverse 1.2.1 ──

    ## ✔ ggplot2 2.2.1.9000     ✔ purrr   0.2.4     
    ## ✔ tibble  1.3.4          ✔ dplyr   0.7.4     
    ## ✔ tidyr   0.7.2          ✔ stringr 1.2.0     
    ## ✔ readr   1.1.1          ✔ forcats 0.2.0

    ## ── Conflicts ─────────────────────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()

``` r
library(colleges)
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
           yield = enrolled / admitted)
```

    ## Warning: Column `ipeds_name`/`school_name` joining character vector and
    ## factor, coercing into character vector

``` r
ggplot(ncaa, aes(x = Year, y = yield)) + 
  geom_point() + geom_line()
```

    ## Warning: Removed 1 rows containing missing values (geom_point).

    ## geom_path: Each group consists of only one observation. Do you need to
    ## adjust the group aesthetic?

![](README_files/figure-markdown_github/unnamed-chunk-2-1.png)
