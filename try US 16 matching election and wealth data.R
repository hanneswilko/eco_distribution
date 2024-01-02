library(dplyr)
library(doBy)
library(tidyverse)
library(Hmisc)
library(tidyr)
library(haven)
library(acid)
library(StatMatch)
getwd()
setwd("C:/Users/ducie/Documents/WU/distribution field/Project")


# Read the Stata data file
data_us16_wi <- read_dta("us16wp (4).dta")

# Set the path to the ZIP file
zip_file_path <- "C:/Users/ducie/Documents/WU/distribution field/Project/Election data/anes_timeseries_2016_dta.zip"

# Set the directory where you want to extract the files
extracted_dir <- "C:/Users/ducie/Documents/WU/distribution field/Project/Election data/"

# Unzip the file
unzip(zip_file_path, exdir = extracted_dir)

# List the files in the extracted directory to confirm the extraction
list.files(extracted_dir)

# Read the Stata dataset into a data frame
anes_data <- read_dta(file.path(extracted_dir, "anes_timeseries_2016.dta"))

# Print the first few rows of the dataset
head(anes_data)
# Create a temporary index by counting the hid values
data_us16_wh$index <- ave(data_us16_wh$hid, data_us16_wh$hid, FUN = seq_along)
data_us16_wi$index <- ave(data_us16_wi$hid, data_us16_wi$hid, FUN = seq_along)

# Merge the data sets by hid and index, keeping all observations from wi
merged_data <- merge(data_us16_wh, data_us16_wi, by = c("hid", "index"), all.y = TRUE)

# Drop the temporary index
merged_data$index <- NULL

#issue is that there are more NA in the dataset because the hid is only 996 for 4980 observations 
