library(tidyverse)
library(Hmisc)
library(tidyr)
library(haven)
library(acid)
library(survey)
library(mitools)
library(convey)
library(dplyr)
library(StatMatch)
library(mice)
library(devtools)
library(srvyr)

#data
data_swe05h <- read.LIS('se05h')
data_swe05p <- read.LIS('se05p')
data_swe05r <- read.LIS('se05r')
swe_df06 <- read_csv(paste(USR_DIR,"/hwilko/cses3_swe06.csv",sep=""))

#merge HH and Individual data
merged05 <- merge(data_swe05h, data_swe05p, by.x = "hid", by.y = "hid", all.x = TRUE)

#create data frame with variables of interest from CSES 
selected_vars <- c("C2003", "C2020","C2010", "C3023_LH_PL")
swe_df06_selected <- swe_df06 %>% 
  select(all_of(selected_vars))

#transforming education categories
merged05 <- merged05 %>%
  mutate(
    education = case_when(
      educlev == "[120]primary" ~ 1,
      educlev == "[130]lower secondary" ~ 2,
      educlev == "[210]upper secondary" ~ 2,
      educlev == "[220]post-secondary non-tertiary" ~ 2,
      educlev == "[311]short-cycle tertiary" ~ 2,
      educlev == "[312]bachelor or equivalent" ~ 3,
      educlev == "[313]master or equivalent" ~ 3,
      educlev == "[320]doctorate or equivalent" ~ 3,
      TRUE ~ NA
    )
  ) %>%
  select(-educlev) 

swe_df06_selected <- swe_df06_selected %>%
  mutate(education = case_when(
    C2003 == 3 ~ 1,
    C2003 == 4 ~ 2, 
    C2003 == 5 ~ 2, 
    C2003 == 7 ~ 3, 
    C2003 == 8 ~ 3,
    C2003 == 98 ~ NA,
    C2003 == 99 ~ NA
  )) %>%
  select(-C2003)

#transforming employment status variable
merged05 <- merged05 %>%
  mutate(
    employment = case_when(
      lfs == "[100]employed" ~ 1,
      lfs == "[200]unemployed" ~ 2,
      lfs == "[300]not in labour force" ~ 2,
      lfs == "[310]retired" ~ 3,
      lfs == "[320]in education" ~ 4,
      lfs == "[330]disabled" ~ 2,
      TRUE ~ NA
    )
  ) %>%
  select(-lfs)

swe_df06_selected <- swe_df06_selected %>%
  mutate(employment = case_when(
    C2010 == 1 ~ 1,
    C2010 == 2 ~ 1,
    C2010 == 3 ~ 1,
    C2010 == 5 ~ 2,
    C2010 == 6 ~ 4,
    C2010 == 7 ~ 3,
    C2010 == 8 ~ 2,
    C2010 == 9 ~ 2,
    C2010 == 10 ~ NA,
    C2010 == 98 ~ NA,
    C2010 == 99 ~ NA
  )) %>%
  select(-C2010)

#transforming income variable
# Create quantiles
quantiles <- quantile(merged05$hitotal, probs = c(0, 0.2, 0.4, 0.6, 0.8, 1))
# Create a new variable with quantile labels
merged05$hitotal_quantile <- cut(merged05$hitotal, breaks = quantiles, labels = FALSE, include.lowest = TRUE)

merged05 <- merged05 %>%
  mutate(
    income = case_when(
      hitotal_quantile == 1 ~ 1,
      hitotal_quantile == 2 ~ 2,
      hitotal_quantile == 3 ~ 3,
      hitotal_quantile == 4 ~ 4,
      hitotal_quantile == 5 ~ 5
    )
  ) %>%
  select(-hitotal_quantile)

swe_df06_selected <- swe_df06_selected %>%
  mutate(income = case_when(
    C2020 == 1 ~ 1,
    C2020 == 2 ~ 2,
    C2020 == 3 ~ 3,
    C2020 == 4 ~ 4,
    C2020 == 5 ~ 5
  )) %>%
  select(-C2020)

# Removing missing values
# Filter missing values for all selected variables
swe_df06_selected1 <- swe_df06_selected[complete.cases(swe_df06_selected[, c("employment", "income", "education")]), ]
merged05_data1 <- merged05[complete.cases(merged05[, c("employment", "income", "education")]), ]

#The following donation classes in data.don are empty, i.e. there are no donors:
##1st-number = employment, 2nd-number = income, 3rd-number = education
'4.2.1 4.3.1 2.5.1 4.5.1'

#Manually cleaning donor classes for matching
## 4.5.1 - If employment 4 (retired), income is 5 and education 1 (low) then employment should become 1 (emp)
swe_df06_selected1$employment <- ifelse(swe_df06_selected1$employment == 4 & swe_df06_selected1$income == 5 & swe_df06_selected1$education == 1, 1, swe_df06_selected1$employment)
merged05_data1$employment <- ifelse(merged05_data1$employment == 4 & merged05_data1$income == 5 & merged05_data1$education == 1, 1, merged05_data1$employment)

## 2.5.1 - If employment 2 (unemp), income is 5 and education 1 (low) then employment should become 1 (emp)
swe_df06_selected1$employment <- ifelse(swe_df06_selected1$employment == 2 & swe_df06_selected1$income == 5 & swe_df06_selected1$education == 1, 1, swe_df06_selected1$employment)
merged05_data1$employment <- ifelse(merged05_data1$employment == 2 & merged05_data1$income == 5 & merged05_data1$education == 1, 1, merged05_data1$employment)

## 4.2.1 - If employment 4 (retired), income is 2 and education 1 (low) then education should become 2 (middle)
swe_df06_selected1$education <- ifelse(swe_df06_selected1$employment == 4 & swe_df06_selected1$income == 2 & swe_df06_selected1$education == 1, 2, swe_df06_selected1$education)
merged05_data1$education <- ifelse(merged05_data1$employment == 4 & merged05_data1$income == 2 & merged05_data1$education == 1, 2, merged05_data1$education)

## 4.3.1 - If employment 4 (retired), income is 2 and education 1 (low) then education should become 2 (middle)
swe_df06_selected1$education <- ifelse(swe_df06_selected1$employment == 4 & swe_df06_selected1$income == 3 & swe_df06_selected1$education == 1, 2, swe_df06_selected1$education)
merged05_data1$education <- ifelse(merged05_data1$employment == 4 & merged05_data1$income == 3 & merged05_data1$education == 1, 2, merged05_data1$education)

#Creating synthetic data set - random hot distance deck matching method
group.v <- c("employment","income","education")

rnd.1 <- RANDwNND.hotdeck(data.rec = merged05_data1, data.don = swe_df06_selected1,
                          match.vars = NULL, don.class = group.v)

fA.rnd.1 <- create.fused(data.rec = merged05_data1, data.don = swe_df06_selected1,
                         mtc.ids = rnd.1$mtc.ids, z.vars = c("C3023_LH_PL"))

#transforming party categories 96,98,99 to NA
fA.rnd.1 <- fA.rnd.1 %>%
  mutate(C3023_LH_PL_modified =
           ifelse(C3023_LH_PL == 90 | C3023_LH_PL == 92 | C3023_LH_PL == 97 | C3023_LH_PL == 98 | C3023_LH_PL == 99, NA, C3023_LH_PL))

unique(fA.rnd.1$C3023_LH_PL_modified)
glimpse(fA.rnd.1)