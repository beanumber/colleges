
## EADA

library(tidyverse)
eada <- read.csv("data-raw/eada/Revenues_All_Sports_and_Men's_Women's_and_Coed_Teams_2003_2004_2005_2006_2007_2008_2009_2010_2011_2012_2013_2014_2015.csv") %>%
  mutate(Year = paste0(Survey.Year, '-', Survey.Year + 1)) %>%
  rename(school_name = Institution.Name, UnitID = UNITID,
         state = State.CD) %>%
  select(Year, UnitID, school_name, Classification.Name,
         Total.Undergraduates, Basketball.Total.Revenue, Football.Total.Revenue)
save(eada, file = "data/eada.rda", compress = "xz")
