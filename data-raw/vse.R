## VSE - donation data

files <- list.files("data-raw/vse", full.names = TRUE)

donations <- load_vse(files[1])

donations <- files %>%
  lapply(load_vse) %>%
  bind_rows() %>%
  rename(Year = year)

## Zimbalist data

public <- readxl::read_xlsx("data-raw/ZimbalistData_07_20_2017.xlsx") %>%
  rename(Year = year)

## Combine donation and Zimbalsit data

donation_total <- donations %>%
  full_join(public, by = c("school_name", "Year")) %>%
  mutate(Year = paste0(Year - 1, "-", Year))

# save
save(donations, file = "data/donations.rda", compress = "xz")
