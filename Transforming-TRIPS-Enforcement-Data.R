library(tidyverse)
library(readxl)

TRIPS.enforcement <- read_excel("/Users/dwaste/Desktop/SOCY-123/CaseStudy-2-Dylan-Waste/TRIPS-enforment.xlsx")

# Create function to convert data and NA into binary for indexing
convert_to_binary <- function(TRIPS.enforcement) {
  binary_df <- data.frame(lapply(TRIPS.enforcement, function(x) ifelse(is.na(x), 0, 1)))
  return(binary_df)
}

# performing binarization
TRIPS.int <- convert_to_binary(TRIPS.enforcement[,4:60])

TRIPS.fin <- cbind(TRIPS.enforcement[,0:3], TRIPS.int)

# Extracting violator state from title
TRIPS.fin <- TRIPS.fin %>%
  mutate(Violator = str_extract(Title, "^[^\\p{L}]*\\p{L}+"))

# Creating depth_index for qualitative measure of enforcement
depth_index <- TRIPS.fin[,4:18] %>%
  rowwise() %>%
  mutate(index = sum(c_across(everything())))

# merging transformed enforcement data sets
TRIPS.fin <- cbind(TRIPS.enforcement[,0:3], depth_index)

# writing file
write_csv(TRIPS.fin, file = "/Users/dwaste/Desktop/SOCY-123/CaseStudy-2-Dylan-Waste/TRIPS-Enforcement-Transformed.csv", progress = show_progress())
