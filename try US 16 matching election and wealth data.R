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


# Read the data file
data_us16_wi <- read_dta("us16wp (4).dta")
data_us16_wh <- read_dta("us16wh (1).dta")

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
                   "V161277", "V161361x", "V160001")
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

library(dplyr)

# Rename V161270 to education and recode values
anes_data_selected <- anes_data_selected %>%
  mutate(
    education = case_when(
      V161270 %in% c(1, 2) ~ 1,
      V161270 == 3 ~ 2,
      V161270 %in% c(4, 5) ~ 3,
      V161270 %in% c(6, 7, 8, 9, 90) ~ 4,
      V161270 == 10 ~ 5,
      V161270 %in% c(11, 12) ~ 6,
      V161270 == 13 ~ 7,
      V161270 %in% c(14, 15) ~ 8,
      V161270 == 16 ~ 9,
      TRUE ~ NA_real_
    )
  ) %>%
  select(-V161270)  # Remove the original variable



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

# Step 1: Convert the variable from character to factor
anes_data_selected$V161361x <- as.factor(anes_data_selected$V161361x)

# Step 2: Rename the variable to "income"
names(anes_data_selected)[names(anes_data_selected) == "V161361x"] <- "income"

# Step 3: Recode the values based on the specified conditions

anes_data_selected <- anes_data_selected %>%
  mutate(
    income = case_when(
      income %in% c("1", "2", "3", "4", "5", "6", "7","8") ~ "1",
      income %in% c("9", "10", "11", "12", "13") ~ "2",
      income %in% c("14", "15", "16", "17", "18", "19") ~ "3",
      income %in% c("20", "21", "22", "23", "24") ~ "4",
      income %in% c("25", "26", "27", "28", "29") ~ "5",
      TRUE ~ "NA"
    )
  )

# Assuming your data frame is named data_us16_wi

library(dplyr)

data_us16_wi <- data_us16_wi %>%
  mutate(
    income = case_when(
      pitotal < 24002 ~ 1,
      pitotal >= 24003 & pitotal <= 45600 ~ 2,
      pitotal >= 45601 & pitotal <= 74869 ~ 3,
      pitotal >= 74870 & pitotal <= 121018 ~ 4,
      pitotal > 121019 ~ 5,
      TRUE ~ NA_real_
    )
  )


# Rename the variable
data_us16_wi$income <- data_us16_wi$pitotal


# Assuming 'hid' is the household identifier and 'pid' is the personal identifier

# Merge household and personal data based on 'hid' and 'pid'
merged_data <- merge(data_us16_wh, data_us16_wi, by.x = "hid", by.y = "hid", all.x = TRUE)

# Print the first few rows of the merged dataset to check
head(merged_data)
