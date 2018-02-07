# IPEDS
library(tidyverse)

ipeds_sat <- read.csv("data-raw/ipeds/Data_7-5-2017.csv") %>%
  gather(key = "variable", value = "value", -UnitID, -Institution.Name) %>%
  mutate(variable = gsub("SAT..I", "SAT.I", variable)) %>%
  filter(variable != "X") %>%
#   filter(Institution.Name == "Duke University") %>%
  # ADM vs. IC (admitted students vs. incoming class?)
  mutate(Year = as.numeric(stringr::str_extract(variable, "[0-9]+[0-9]+[0-9]+[0-9]+")),
         Year = ifelse(Year < 2000, floor(Year / 100) + 2001, Year),
         Year = paste0(Year, "-", Year + 1),
         var_name = stringr::str_sub(variable, 1, -10),
         var_name = gsub("SAT.I", "SAT", var_name),
         var_name = gsub("Verbal.", "Critical.Reading.", var_name),
         var_name = gsub("total.", "total", var_name),
         var_name = gsub("score.", "score", var_name)) %>%
  rename(school_name = Institution.Name) %>%
  select(-variable) %>%
  # Kennesaw State
  filter(Year > 0, UnitID != 486840) %>%
  spread(key = var_name, value = value) %>%
  select(-4)

## IPEDS - Tuition and fees, comprehensive fee

ipeds_fees <- read.csv("data-raw/ipeds/Data_8-7-2017.csv") %>%
  #In.state.comprehensive.feer.full.time.undergraduates. = In.state.comprehensive.fee
  # Out.of.state.comprehensive.feer.full.time.undergraduates. = Out.of.state.comprehensive.fee
  # Verified by examples : Bates College, Connecticut College
  select(-contains("undergraduates")) %>%
  gather(key = "variable", value = "value", -UnitID, -Institution.Name) %>%
  #distinct(ipeds2, var_name)
  mutate(Year = as.numeric(stringr::str_extract(variable, "[0-9]+[0-9]+[0-9]+[0-9]+")),
         var_name = stringr::str_sub(variable, 1, -12),
         var_name = gsub("[[:digit:]]", "", var_name),
         var_name = gsub("Published", "", var_name),
         var_name = gsub("\\.\\.", "\\.", var_name),
         var_name = gsub("\\.in", "In", var_name),
         var_name = gsub("\\.out", "Out", var_name)) %>%
  #         var_name = gsub("Published.in.state.tuition.and.fees...", "in.state.tuition.and.fees", var_name),
  #         var_name = gsub("Published.out.of.state.tuition.and.fees...", "out.of.state.tuition.and.fees",var_name),
  #         var_name = gsub("In.state.comprehensive.fee...", "In.state.comprehensive.fee", var_name),
  #         var_name = gsub("Out.of.state.comprehensive.fee...", "Out.of.state.comprehensive.fee", var_name),
  #         var_name = gsub("Published.in.state..tuition.and.fees...", "in.state.tuition.and.fees", var_name),
  #         var_name = gsub("Published.out.of.state..tuition.and.fees...", "out.of.state.tuition.and.fees", var_name)) %>%
  filter(var_name > "") %>%
  select(-variable) %>%
  spread(key = var_name, value = value) %>%
  mutate(Year = paste0(Year, "-", Year + 1))
## the spread function doesn't work due to "Duplicate identifiers error"


## IPEDS - combine

ipeds <- ipeds_sat %>%
#  filter(school_name == "Duke University")
  left_join(ipeds_fees, by = c("UnitID", "Year"))

ipeds %>%
  group_by(Year) %>%
  summarize(num_units = n_distinct(UnitID), num_schools = n())

# save(ipeds, file = "data/ipeds.rda", compress = "xz")



## EADA

library(tidyverse)
eada <- read.csv("data-raw/eada/Revenues_All_Sports_and_Men's_Women's_and_Coed_Teams_2003_2004_2005_2006_2007_2008_2009_2010_2011_2012_2013_2014_2015.csv") %>%
  mutate(Year = paste0(Survey.Year, '-', Survey.Year + 1)) %>%
  rename(school_name = Institution.Name, UnitID = UNITID,
         state = State.CD,
         division = Classification.Name) %>%
  select(Year, UnitID, school_name, state, division,
         Total.Undergraduates, Grand.Total.Revenue,
         Basketball.Total.Revenue, Football.Total.Revenue)
# save(eada, file = "data/eada.rda", compress = "xz")

eada %>%
  mutate(bb_pct = Basketball.Total.Revenue / Grand.Total.Revenue,
         fb_pct = Football.Total.Revenue / Grand.Total.Revenue,
         other_pct = 1 - bb_pct - fb_pct) %>%
  ggplot(aes(x = other_pct)) +
  geom_density()

## JOIN ipeds and EADA data

institutions <- ipeds %>%
  select(-school_name) %>%
  right_join(eada, by = c("UnitID", "Year")) %>%
  select(-Institution.Name)
institutions %>%
  group_by(Classification.Name) %>%
  summarize(num_schools = n_distinct(UnitID))

save(institutions, file = "data/institutions.rda", compress = "xz")

## School lookup

#temp_lkup <- institutions %>%
#  group_by(UnitID) %>%
#  summarize(N = n_distinct(school_name), names = paste(unique(school_name), collapse = ","))

schools <- readr::read_csv("data-raw/school_lkup.csv")
#schools <- schools %>%
#  left_join(temp_lkup, by = c("ipeds_name" = "names")) %>%
#  select(UnitID, ipeds_name, sports_ref_name)

save(schools, file = "data/schools.rda", compress = "xz")


