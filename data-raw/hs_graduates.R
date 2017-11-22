
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
  select(-c(X2:X5,X7,X9, X18:X28))
# head<-highschool_data1[5,]
highschool_data1 <- highschool_data1[14:74,]
colnames(highschool_data1) <- c("State", "2007", "2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015", "2016")



## data before 2007

highschool_url2 <- paste0("https://nces.ed.gov/programs/digest/d12/tables/dt12_123.asp")
highschool_data2 <- read_html(highschool_url2) %>%
  html_nodes("table") %>%
  html_table(header = FALSE) %>%
  bind_rows() %>%
  filter(X1 != "") %>%
  select(-c(X2:X8,X10,X12:X18))
# head<-highschool_data2[4,]
highschool_data2 <- highschool_data2[7:67,]
colnames(highschool_data2) <- c("State", "2005", "2006")


## Combine
hs_graduates <- highschool_data2 %>%
  full_join(highschool_data1, by = "State") %>%
  gather(key = "Year", value = "hs_diplomas", -State) %>%
  mutate(hs_diplomas = parse_number(hs_diplomas),
         Year = as.numeric(Year),
         Year = paste0(Year - 1, "-", Year)) %>%
  filter(!is.na(hs_diplomas))

save(hs_graduates, file = "data/hs_graduates.rda", compress = "xz")
```
