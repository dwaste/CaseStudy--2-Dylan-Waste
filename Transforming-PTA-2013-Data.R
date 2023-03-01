library(tidyverse)
library(readxl)
library(haven)

# reading in data from:
# Dür, Andreas, Leonardo Baccini, and Manfred Elsig. 2013. 
#   “The Design of International Trade Agreements: Introducing a New Dataset.” 
#   The Review of International Organizations 9(3): 353–75. 
pta.2013 <- read_dta("/Users/dwaste/Desktop/SOCY-123/pta-2013.dta")

# grouping by PTA agreement and summarizing depth over total duration of agreement
pta.2013.transformed <- pta.2013 %>%
  filter(depth_index != 0) %>%
  group_by(id) %>%
  mutate(avg_depth_index = sum(depth_index/n())) %>%
  distinct(country_a, country_b, .keep_all = TRUE)

# selecting relevant data
pta.2013.summary <- pta.2013.transformed %>%
  select(c("country_a", "country_b", "avg_depth_index", "gattwto", "contig", "armconflict", "id",
          "religion", "distln", "comlang_off", "comcol", "comcur", "comleg"))

# Writing finished file
write_csv(TRIPS, file = "/Users/dwaste/Desktop/SOCY-123/CaseStudy-2-Dylan-Waste/PTA-2013-Transformed.csv", progress = show_progress())
