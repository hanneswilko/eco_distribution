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
