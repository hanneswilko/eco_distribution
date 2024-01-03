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

#create data with variables of interest

selected_vars <- c("V161109", "V161268", "V161316", "V161324", "V161334", "V162024a", 
                   "V162034a", "V162292a", "V161270", "V161342", "V161267", 
                   "V161277", "V161361x")
anes_data_selected <- anes_data %>% 
  select(all_of(selected_vars))

#transforming the education variable for future matching
data_us16_wi <- data_us16_wi %>%
  mutate(
    education = case_when(
      educlev %in% c(111, 110) ~ 1,
      educlev == 120 ~ 2,
      educlev == 130 ~ 3,
      educlev == 210 ~ 4,
      educlev == 220 ~ 5,
      educlev == 311 ~ 6,
      educlev == 312 ~ 7,
      educlev == 313 ~ 8,
      educlev == 320 ~ 9,
      TRUE ~ NA_integer_
    )
  ) %>%
  select(-educlev)  # Remove the original variable

#transform gender variable for future matching
anes_data_selected <- anes_data_selected %>%
  mutate(
    gender = case_when(
      V161342 %in% c(3, -9) ~ NA_integer_,
      TRUE ~ V161342
    )
  ) %>%
  select(-V161342)  

data_us16_wi <- data_us16_wi %>%
  rename(gender = sex)

#for age just need to rename
anes_data_selected <- anes_data_selected %>%
  rename(age = V161267)

#modify the employment status variable in both sets
anes_data_selected <- anes_data_selected %>%
  mutate(
    employment = case_when(
      V161277 == 1 ~ 1,
      V161277 == 4 ~ 2,
      V161277 == 5 ~ 3,
      V161277 == 8 ~ 4,
      V161277 == 6 ~ 5,
      V161277 == 7 ~ 6,
      V161277 %in% c(2, -9) ~ NA_integer_,
      TRUE ~ NA_integer_
    )
  ) %>%
  select(-V161277)

data_us16_wi <- data_us16_wi %>%
  mutate(
    employment = case_when(
      lfs == 100 ~ 1,
      lfs == 200 ~ 2,
      lfs == 310 ~ 3,
      lfs == 320 ~ 4,
      lfs == 330 ~ 5,
      lfs == 340 ~ 6,
      lfs == 300 ~ NA_integer_,
      TRUE ~ NA_integer_
    )
  ) %>%
  select(-lfs)

#modify the income variables to express it in income quintile
if (inherits(anes_data_selected$V161361x, "labelled")) {
  anes_data_selected$V161361x <- as.character(as_factor(anes_data_selected$V161361x))
} else {
  anes_data_selected$V161361x <- as.character(anes_data_selected$V161361x)
}

# Rename the variable
anes_data_selected$income <- anes_data_selected$V161361x

# Recode the values
anes_data_selected$income[anes_data_selected$income %in% c("01", "02", "03", "04", "05", "06", "07", "08")] <- "1"
anes_data_selected$income[anes_data_selected$income %in% c("09","10","11", "12", "13")] <- "2"
anes_data_selected$income[anes_data_selected$income %in% c("14", "15","16", "17","18", "19")] <- "3"
anes_data_selected$income[anes_data_selected$income %in% c("20","21", "22", "23", "24")] <- "4"
anes_data_selected$income[anes_data_selected$income %in% c("25", "26", "27", "28", "29")] <- "5"
anes_data_selected$income[anes_data_selected$income == "-9"] <- NA_character_ # or any other value you prefer for -9

# Assuming your data frame is named data_us16_wi

# Rename the variable
data_us16_wi$income <- data_us16_wi$pitotal

# Recode the values
data_us16_wi$income[data_us16_wi$income < 24002] <- 1
data_us16_wi$income[data_us16_wi$income >= 24003 & data_us16_wi$income <= 45600] <- 2
data_us16_wi$income[data_us16_wi$income >= 45601 & data_us16_wi$income <= 74869] <- 3
data_us16_wi$income[data_us16_wi$income >= 74870 & data_us16_wi$income <= 121018] <- 4
data_us16_wi$income[data_us16_wi$income > 121019] <- 5

#next step is the matching between anes_data_selected and data_us16_wi

# Assuming 'hid' is the household identifier and 'pid' is the personal identifier

# Merge household and personal data based on 'hid' and 'pid'
merged_data <- merge(data_us16_wh, data_us16_wi, by.x = "hid", by.y = "hid", all.x = TRUE)

# Print the first few rows of the merged dataset to check
head(merged_data)
