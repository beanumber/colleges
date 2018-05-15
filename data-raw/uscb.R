
## US Census - Income & Income per capita

## apply to data from all the 11 years

files <- list.files("data-raw/uscb", full.names = TRUE)

x <- load_uscb(files[6])

incomes <- lapply(files, FUN = load_uscb) %>%
  bind_rows()

# sanity check

ggplot(incomes, aes(x = acad_end_year, y = per_capita_income, color = State)) +
  geom_point() +
  geom_line() +
  scale_y_continuous(labels = scales::dollar)

# save(incomes, file = "data/incomes.rda", compress = "xz")



## Number of High School Diploma by States


## data since 2007
library(tidyverse)
library(rvest)

highschool_url1 <- paste0("https://nces.ed.gov/programs/digest/d16/tables/dt16_219.20.asp")
highschool_data1 <- read_html(highschool_url1) %>%
  html_nodes("table") %>%
  html_table(header = FALSE, fill=TRUE) %>%
  bind_rows() %>%
  filter(X1 != "") %>%
  select(-c(X2:X5, X7, X9, X18:X28))
# head<-highschool_data1[5,]
highschool_data1 <- highschool_data1[14:74, ]
colnames(highschool_data1) <- c("State", 2007:2016)



## data before 2007

highschool_url2 <- paste0("https://nces.ed.gov/programs/digest/d12/tables/dt12_123.asp")
highschool_data2 <- read_html(highschool_url2) %>%
  html_nodes("table") %>%
  html_table(header = FALSE) %>%
  bind_rows() %>%
  filter(X1 != "") %>%
  select(-c(X2:X8, X10, X12:X18))
# head<-highschool_data2[4,]
highschool_data2 <- highschool_data2[7:67,]
colnames(highschool_data2) <- c("State", 2005:2006)


## Combine
hs_graduates <- highschool_data2 %>%
  full_join(highschool_data1, by = "State") %>%
  gather(key = "Year", value = "hs_diplomas", -State) %>%
  mutate(State = gsub("[0-9]+", "", State),
         hs_diplomas = parse_number(hs_diplomas),
         acad_end_year = as.numeric(Year)) %>%
  filter(!is.na(hs_diplomas))

# save(hs_graduates, file = "data/hs_graduates.rda", compress = "xz")

state_lkup <- data.frame(abb = state.abb, name = state.name) %>%
  tibble::add_row(abb = "DC", name = "District of Columbia") %>%
  tibble::add_row(abb = "PR", name = "Puerto Rico")

states <- incomes %>%
  inner_join(hs_graduates, by = c("State", "acad_end_year")) %>%
  left_join(state_lkup, by= c("State" = "name"))

states %>%
  group_by(acad_end_year) %>%
  summarize(num_states = n())


save(states, file = "data/states.rda", compress = "xz")
