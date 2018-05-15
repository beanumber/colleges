## VSE - donation data

files <- list.files("data-raw/vse", full.names = TRUE)

donations <- load_vse(files[1])

donations <- files %>%
  lapply(load_vse) %>%
  bind_rows() %>%
  rename(Year = year)

donations %>%
  group_by(Year) %>%
  summarize(num_schools = n_distinct(school_name))

donations$school_name %>% unique() %>% sort()

## Zimbalist data

public <- readxl::read_xlsx("data-raw/ZimbalistData_07_20_2017.xlsx") %>%
  rename(acad_end_year = year, zimb_total = total_contributions) %>%
  mutate(zimb_total = ifelse(is.na(zimb_total),
                             contributions_8 + in_kind_contributions_9,
                             zimb_total)) %>%
  left_join(schools, by = c("school_name" = "sports_ref_name")) %>%
  select(acad_end_year, ipeds_name, zimb_total)

## Combine donation and Zimbalist data

# donation_total <- donations %>%
#   full_join(public, by = c("school_name", "Year")) %>%
#   mutate(Year = paste0(Year - 1, "-", Year))

## New Zimbalist data

read_and_clean <- function(x, ...) {
  readr::read_csv(x, ...) %>%
    mutate(acad_end_year = readr::parse_number(Year),
           grand_total = parse_number(`Grand Total`),
#           alumni_total = parse_number(`Alumni: Total`),
           athletics_total = parse_number(`Athletics/Total: Total Amount ($)`)) %>%
    rename(donations_name = Institution)
}

# superseded
aau_small <- read_and_clean("data-raw/donations_aau_small.csv")

aau_big <- read_and_clean("data-raw/donations_aau.csv") %>%
  select(acad_end_year, donations_name, grand_total, athletics_total)

midwest <- read_and_clean("data-raw/donations_midwest.csv") %>%
  mutate(total_current_ops = parse_number(`Total Current Operations`)) %>%
  select(acad_end_year, donations_name, grand_total, athletics_total, total_current_ops)

large <- read_and_clean("data-raw/donations_big.csv") %>%
  select(acad_end_year, donations_name, State, grand_total, athletics_total)

schools_donations <- read_csv("data-raw/schools_donations.csv")

donations <- large %>%
  full_join(midwest, by = c("acad_end_year", "donations_name")) %>%
  full_join(aau_big, by = c("acad_end_year", "donations_name")) %>%
  filter(!is.na(acad_end_year)) %>%
  mutate(grand_total = pmax(grand_total.x, grand_total.y, grand_total, na.rm = TRUE),
         athletics_total = pmax(athletics_total.x, athletics_total.y, athletics_total, na.rm = TRUE)) %>%
  select(-ends_with(".x"), -ends_with(".y")) %>%
  left_join(schools_donations, by = c("donations_name")) %>%
  mutate(school_name = ifelse(is.na(ipeds_name), donations_name, ipeds_name)) %>%
  left_join(schools, by = c("school_name" = "ipeds_name")) %>%
  select(-donations_name) %>%
  arrange(school_name, acad_end_year) %>%
  distinct()

donations %>%
  filter(is.na(sports_ref_name)) %>%
  group_by(school_name) %>%
  summarize(N = n(), dollars = sum(grand_total)) %>%
  arrange(desc(dollars))

#donations <- zimb_new %>%
#  left_join(schools, by = c("school_name" = "ipeds_name")) %>%
#  full_join(public, by = c("school_name" = "ipeds_name", "acad_end_year" = "acad_end_year")) %>%

# donations %>%
#   filter(is.na(sports_ref_name)) %>%
#   select(school_name, sports_ref_name) %>%
#   unique() %>%
#   print(n = Inf)
#
# donations %>%
#   filter(!is.na(zimb_total)) %>%
#   select(sports_ref_name) %>%
#   unique() %>%
#   print(n = Inf)

donations %>%
  group_by(acad_end_year) %>%
  summarize(N = n(), matched = sum(!is.na(UnitID)),
            num_grand = sum(!is.na(grand_total)),
            num_athletics = sum(!is.na(athletics_total)))

ggplot(donations, aes(x = grand_total, y = athletics_total)) +
  geom_point() +
  scale_x_log10() + scale_y_log10()

# save
save(donations, file = "data/donations.rda", compress = "xz")
# write.csv(donations, file = "data-raw/donations.csv", row.names = FALSE)
