
## US Census - Income & Income per capita

## apply to data from all the 11 years

files <- list.files("data-raw/uscb", full.names = TRUE)

x <- load_uscb(files[6])

incomes <- lapply(files, FUN = load_uscb) %>%
  bind_rows()

# sanity check

ggplot(incomes, aes(x = Year, y = per_capita_income, color = State)) +
  geom_point() +
  geom_line() +
  scale_y_continuous(labels = scales::dollar)

save(incomes, file = "data/incomes.rda", compress = "xz")
