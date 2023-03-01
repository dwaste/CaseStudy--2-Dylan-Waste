library(tidyverse)
library(readxl)
library(countrycode)
library(wbstats)

wb_search("imports")

TRIPS.enforcement <- read_csv("/Users/dwaste/Desktop/SOCY-123/CaseStudy-2-Dylan-Waste/TRIPS-Enforcement-Transformed.csv") %>%
  

PTA.2013 <- read_csv("/Users/dwaste/Desktop/SOCY-123/CaseStudy-2-Dylan-Waste/PTA-2013-Transformed.csv")

test.df <- full_join(PTA.2013, TRIPS.enforcement, by = c("country_a" = "Complainant"), na_matches = "never")

glimpse(test.df)