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

zimb_new <- readr::read_csv("data-raw/DIDonations.csv") %>%
  mutate(acad_end_year = readr::parse_number(Year),
         grand_total = parse_number(`Grand Total`),
         alumni_total = parse_number(`Alumni: Total`),
         athletics_total = parse_number(`Athletics/Total: Total Amount ($)`)) %>%
  rename(school_name = Institution) %>%
  mutate(school_name = gsub("Columbia University", "Columbia University in the City of New York", school_name),
         school_name = gsub("Indiana University", "Indiana University-Bloomington", school_name),
         school_name = gsub("Ohio State University", "Ohio State University-Main Campus", school_name),
         school_name = gsub("Penn State University", "Pennsylvania State University-Main Campus", school_name),
         school_name = gsub("Purdue University", "Purdue University-Main Campus", school_name),
         school_name = gsub("Rutgers University", "Rutgers University-New Brunswick", school_name),
         school_name = gsub("Texas A&M University", "Texas A & M University-College Station", school_name),
         school_name = gsub("University of Colorado", "University of Colorado Boulder", school_name),
         school_name = gsub("University of Michigan", "University of Michigan-Ann Arbor", school_name),
         school_name = gsub("University of Minnesota", "University of Minnesota-Twin Cities", school_name),
         school_name = gsub("University of Texas at Austin", "The University of Texas at Austin", school_name),
         school_name = gsub("University of Washington", "University of Washington-Seattle Campus", school_name)) %>%
  select(acad_end_year, school_name, grand_total, alumni_total, athletics_total)
donations <- zimb_new %>%
  left_join(schools, by = c("school_name" = "ipeds_name")) %>%
  full_join(public, by = c("school_name" = "ipeds_name", "acad_end_year" = "acad_end_year")) %>%
  select(-sports_ref_name) %>%
  arrange(school_name, acad_end_year)

donations %>%
  filter(is.na(sports_ref_name)) %>%
  select(school_name, sports_ref_name) %>%
  unique() %>%
  print(n = Inf)

donations %>%
  filter(!is.na(zimb_total)) %>%
  select(sports_ref_name) %>%
  unique() %>%
  print(n = Inf)

donations %>%
  group_by(acad_end_year) %>%
  summarize(N = n(), num_vse = sum(!is.na(grand_total)),
            num_zimb = sum(!is.na(zimb_total)))

ggplot(donations, aes(x = grand_total, y = zimb_total)) +
  geom_point()

# save
save(donations, file = "data/donations.rda", compress = "xz")
# write.csv(donations, file = "data-raw/donations.csv", row.names = FALSE)
