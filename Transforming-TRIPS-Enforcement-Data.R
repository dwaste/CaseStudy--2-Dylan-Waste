library(tidyverse)
library(readxl)

TRIPS.enforcement <- read_excel("/Users/dwaste/Desktop/SOCY-123/CaseStudy-2-Dylan-Waste/TRIPS-enforcement.xlsx")

# Create function to convert data and NA into binary for indexing
convert_to_binary <- function(TRIPS.enforcement) {
  binary_df <- data.frame(lapply(TRIPS.enforcement, function(x) ifelse(is.na(x), 0, 1)))
  return(binary_df)
}

# performing binarization
TRIPS.int <- convert_to_binary(TRIPS.enforcement[,4:60])

TRIPS.int <- cbind(TRIPS.enforcement[,0:3], TRIPS.int)

# Extracting violator state from title
TRIPS.vio <- TRIPS.int %>%
  group_by(Title) %>%
  mutate(Violator = str_extract(Title, "(?<=^|–\\s)([A-Za-z ]+)(?=\\s-\\s)")) %>%
  mutate(Violator = recode(Violator, "US" = "United States",
                           "Russian Federation" = "Russia"))

unique(TRIPS.vio$Violator)

# Creating depth_index for qualitative measure of enforcement
index <- TRIPS.vio[,4:18] %>%
  rowwise() %>%
  mutate(depth_index = sum(c_across(everything())))

# merging transformed enforcement data sets
TRIPS.dep <- cbind(TRIPS.enforcement, index)

TRIPS.fin <- cbind(TRIPS.dep, TRIPS.vio$Violator)

# formatting final dataset
TRIPS <- TRIPS.fin %>%
  select(c("Complainant", "Title", "depth_index", "TRIPS.vio$Violator")) %>%
  rename(Violator = `TRIPS.vio$Violator`) %>%
  na.omit()

# writing file
write_csv(TRIPS, file = "/Users/dwaste/Desktop/SOCY-123/CaseStudy-2-Dylan-Waste/TRIPS-Enforcement-Transformed.csv", progress = show_progress())
