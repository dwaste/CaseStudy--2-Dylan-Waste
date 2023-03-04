library(tidyverse)
library(readxl)
library(countrycode)
library(stargazer)

# reading in data
TRIPS.enforcement <- read_csv("/Users/dwaste/Desktop/SOCY-123/CaseStudy-2-Dylan-Waste/TRIPS-Enforcement-Transformed.csv")
PTA.2013 <- read_csv("/Users/dwaste/Desktop/SOCY-123/CaseStudy-2-Dylan-Waste/PTA-2013-Transformed.csv")
export.dat <- read_excel("/Users/dwaste/Desktop/SOCY-123/CaseStudy-2-Dylan-Waste/Exports_and_Imports_by_Areas_and_Countries.xlsx", sheet = "Exports")
import.dat <- read_excel("/Users/dwaste/Desktop/SOCY-123/CaseStudy-2-Dylan-Waste/Exports_and_Imports_by_Areas_and_Countries.xlsx", sheet = "Imports")
       
TRIPS.enforcement$Title
# pivot ts data into a long format  
export.dat <- export.dat %>%
  pivot_longer(cols = starts_with("2014"), names_to = "date", values_to = "value") %>%
  mutate(date = lubridate::ymd(paste0(date, "01")))

# calculate the average value per country
export.avg <- export.dat %>%
  group_by(Country) %>%
  summarise(avg_export = mean(value))

# pivot ts data into a long format
import.dat <- import.dat %>%
  pivot_longer(cols = starts_with("2014"), names_to = "date", values_to = "value") %>%
  mutate(date = lubridate::ymd(paste0(date, "01")))

# calculate the average value per country
import.avg <- import.dat %>%
  group_by(Country) %>%
  summarise(avg_import = mean(value))

# joining data for avg export/import from 2014M01 - 2017M12
econ.fin <- full_join(export.avg, import.avg, by = "Country")

econ.fin <- econ.fin %>%
  mutate(Country = recode(Country, "Türkiye, Rep of" = "Turkey",
                          "Aruba, Kingdom of the Netherlands" = "Aruba",
                          "Curaçao, Kingdom of the Netherlands" = "Curaçao")) %>%
  mutate(country_code = countrycode(Country, origin = "country.name", destination = "iso3c"))

# creating summary df for enforcement data by violating country
TRIPS.fin <- TRIPS.enforcement %>%
  group_by(Violator) %>%
  mutate(avg_enforcement_depth = mean(depth_index)) %>%
  mutate(country_code = countrycode(Violator, origin = "country.name", destination = "iso3c")) %>%
  select(c("Violator", "avg_enforcement_depth", "country_code")) %>%
  distinct()

# creating summary df for PTA data by country A
PTA.fin <- PTA.2013 %>%
  group_by(country_a) %>%
  mutate(avg_depth_index = mean(avg_depth_index)) %>%
  mutate(avg_polity2_A = mean(polity2_A)) %>%
  mutate(avg_polity2_B = mean(polity2_B)) %>%
  mutate(avg_gattwto = mean(gattwto)) %>%
  mutate(avg_contig = mean(contig)) %>%
  mutate(avg_armconflict = mean(armconflict)) %>%
  mutate(avg_religion = mean(religion)) %>%
  mutate(avg_distln = mean(distln)) %>%
  mutate(avg_contig = mean(avg_contig)) %>%
  mutate(avg_comlang_off = mean(comlang_off)) %>%
  mutate(avg_comcol = mean(comcol)) %>%
  mutate(avg_comcur = mean(comcur)) %>%
  mutate(avg_comleg = mean(comleg)) %>%
  select(c("country_a","avg_depth_index", "avg_polity2_A", "avg_polity2_B", "avg_gattwto", "avg_contig", "avg_armconflict",
           "avg_religion", "avg_distln", "avg_contig", "avg_comlang_off", "avg_comcol", "avg_comcur", "avg_comleg")) %>%
  distinct()


# joining datasets
ana.dat <- right_join(TRIPS.fin, econ.fin, by = "country_code", multiple = "all")

ana.dat <- left_join(ana.dat, PTA.fin, by = c("country_code" = "country_a"))

# finalizing DV for analysis 
ana.dat <- ana.dat %>%
  mutate(avg_enforcement_depth = if_else(is.na(avg_enforcement_depth), 0, avg_enforcement_depth))

# finding variables for linear models
glimpse(ana.dat)

# creating models
mod1 <- lm(formula = avg_enforcement_depth ~ avg_import + avg_export, data = ana.dat)
mod2 <- lm(formula = avg_enforcement_depth ~ avg_import + avg_export + avg_depth_index, data = ana.dat)
mod3 <- lm(formula = avg_enforcement_depth ~ avg_import + avg_export + avg_depth_index + avg_gattwto + avg_polity2_A + avg_polity2_B, data = ana.dat)
mod4 <- lm(formula = avg_enforcement_depth ~ avg_import + avg_export + avg_depth_index + avg_gattwto + avg_polity2_A + avg_polity2_B + avg_contig + avg_distln, data = ana.dat)
mod5 <- lm(formula = avg_enforcement_depth ~ avg_depth_index + avg_gattwto + avg_polity2_A + avg_polity2_B + avg_contig + avg_distln, data = ana.dat)

# creating regression table
stargazer(mod1,mod2,mod3,mod4,mod5, type = "html", out = "TRIPS-enforcement-models-table.html",
          title = "My TRIPS Enforcement Models")

