library(tidyverse)
library(colleges)

ncaa <- sports %>%
  left_join(schools, by = c("school_name" = "sports_ref_name")) %>%
  left_join(institutions, by = c("ipeds_name" = "school_name", "Year")) %>%
  mutate(acad_end_year = as.numeric(stringr::str_sub(Year, -4)),
         is_military = ifelse(school_name %in% c("Air Force", "Army", "Navy"), TRUE, FALSE),
         is_private = ifelse(is.na(`State.appropriations..F`), TRUE,
                             ifelse(`State.appropriations..F` > 0, FALSE, TRUE)),
         state_funding = `State.appropriations..F`,
         dollars_per_capita = `State.appropriations..F` / Enrolled.total,
         enrolled = Enrolled.total,
         admitted = Admissions.total,
         applied = Applicants.total,
         yield = enrolled / admitted,
         admit_rate = admitted / applied,
         ugrads = Total.Undergraduates,
         comp_fee_in_state = `In.state.comprehensive.fee..`,
         comp_fee_out_state = `Out.of.state.comprehensive.fee..`,
         bball_revenue = Basketball.Total.Revenue,
         fball_revenue = Football.Total.Revenue,
         #           act_composite_25 = ACT.Composite.25th.percentile.score,
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
  left_join(donations, by = c("acad_end_year", "ipeds_name" = "school_name")) %>%
  select(acad_end_year, Year, school_name, is_military, is_private, state_funding, dollars_per_capita,
         applied, admitted, enrolled, yield, admit_rate,
         #           act_composite_25,
         act_composite_75, sat_25_avg, sat_75_avg,
         division, state,
         ugrads, comp_fee_in_state, comp_fee_out_state,
         bball_revenue, fball_revenue,
         grand_total, alumni_total, athletics_total, zimb_total,
         bb_conf, fb_conf,
         bball_wins, bball_losses, bball_wpct, bb_final_ranking, bb_champs, bb_final_four, tourney, conf_champ, conf_tourney,
         fball_wins, fball_losses, fball_wpct, fb_final_ranking, fb_champs, fb_final_four)



save(ncaa, file = "data/ncaa.rda", compress = "xz")

fbs <- ncaa %>%
  filter(acad_end_year < 2017,
         !is.na(fball_losses))

power_five_schools <- fbs %>%
  filter(bb_conf %in% c("ACC", "Big Ten", "Big 12", "Pac-10", "Pac-12", "SEC", "SEC (West)", "SEC (East)")) %>%
  pull(school_name) %>%
  unique()

fbs <- fbs %>%
  mutate(power_five = school_name %in% power_five_schools)

save(fbs, file = "data/fbs.rda", compress = "xz")

power5 <- fbs %>%
  filter(power_five)

power5 %>%
  group_by(acad_end_year) %>%
  summarize(N = n(), sum(is.na(grand_total)))

save(power5, file = "data/power5.rda", compress = "xz")

## Reconcile donations
fbs
x <- power5 %>%
  select(acad_end_year, school_name, grand_total, athletics_total, alumni_total, zimb_total)
  group_by(acad_end_year) %>%
  summarize(N = n(), num_gt = sum(!is.na(grand_total)),
            num_zimb = sum(!is.na(zimb_total)))




