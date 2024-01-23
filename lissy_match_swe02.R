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
data_swe02h <- read.LIS('se02h')
data_swe02p <- read.LIS('se02p')
data_swe02r <- read.LIS('se02r')
swe_df02 <- read_csv(paste(USR_DIR,"/hwilko/cses2_swe02.csv",sep=""))

#merge HH and Individual data
merged02 <- merge(data_swe02h, data_swe02p, by.x = "hid", by.y = "hid", all.x = TRUE)

#create data frame with variables of interest from CSES 
selected_vars <- c("B2003", "B2020","B2010", "B3006_1")
swe_df02_selected <- swe_df02 %>% 
  select(all_of(selected_vars))

#transforming education categories
merged02 <- merged02 %>%
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

swe_df02_selected <- swe_df02_selected %>%
  mutate(education = case_when(
    B2003 == 3 ~ 1,
    B2003 == 4 ~ 2, 
    B2003 == 5 ~ 2, 
    B2003 == 7 ~ 3, 
    B2003 == 8 ~ 3,
    B2003 == 98 ~ NA
  )) %>%
  select(-B2003)

#transforming employment status variable
merged02 <- merged02 %>%
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

swe_df02_selected <- swe_df02_selected %>%
  mutate(employment = case_when(
    B2010 == 1 ~ 1,
    B2010 == 2 ~ 1,
    B2010 == 3 ~ 1,
    B2010 == 5 ~ 2,
    B2010 == 6 ~ 4,
    B2010 == 7 ~ 3,
    B2010 == 8 ~ 2,
    B2010 == 10 ~ NA,
    B2010 == 97 ~ NA,
    B2010 == 98 ~ NA,
    B2010 == 99 ~ NA
  )) %>%
  select(-B2010)

#transforming income variable
merged02 <- merged02 %>%
  mutate(
    income = case_when(
      hitotal < 61131 ~ 1,
      hitotal >= 61131 & hitotal <= 142218 ~ 2,
      hitotal >= 142219 & hitotal <= 214785 ~ 3,
      hitotal >= 214786 & hitotal <= 290802 ~ 4,
      hitotal > 290802 ~ 5,
      TRUE ~ NA
    )
  )

swe_df02_selected <- swe_df02_selected %>%
  mutate(income = case_when(
    B2020 == 1 ~ 1,
    B2020 == 2 ~ 2,
    B2020 == 3 ~ 3,
    B2020 == 4 ~ 4,
    B2020 == 5 ~ 5,
    B2020 == 7 ~ NA,
    B2020 == 8 ~ NA,
    B2020 == 9 ~ NA,
  )) %>%
  select(-B2020)

# Removing missing values
swe_df02_selected1 <- swe_df02_selected[complete.cases(swe_df02_selected$employment), ]
swe_df02_selected1 <- swe_df02_selected[complete.cases(swe_df02_selected$income), ]
swe_df02_selected1 <- swe_df02_selected[complete.cases(swe_df02_selected$education), ]

merged02_data1 <- merged02[complete.cases(merged02$employment), ]
merged02_data1 <- merged02[complete.cases(merged02$income), ]
merged02_data1 <- merged02[complete.cases(merged02$education), ]

#The following donation classes in data.don are empty, i.e. there are no donors:
##1st-number = employment, 2nd-number = income, 3rd-number = education
'2.4.1 2.5.1 2.4.3 4.5.3 4.5.1 4.5.2'

#Manually cleaning donor classes for matching
## 4.5.1 - If employment 4 (retired), income is 5 and education 1 (low) then employment should become 1 (emp)
swe_df02_selected1$employment <- ifelse(swe_df02_selected1$employment == 4 & swe_df02_selected1$income == 5 & swe_df02_selected1$education == 1, 1, swe_df02_selected1$employment)
merged02_data1$employment <- ifelse(merged02_data1$employment == 4 & merged02_data1$income == 5 & merged02_data1$education == 1, 1, merged02_data1$employment)

## 4.5.2 - If employment 1 (emp), income is 5 and education 1 (low) then employment should become 1 (emp)
swe_df02_selected1$employment <- ifelse(swe_df02_selected1$employment == 4 & swe_df02_selected1$income == 5 & swe_df02_selected1$education == 2, 1, swe_df02_selected1$employment)
merged02_data1$employment <- ifelse(merged02_data1$employment == 4 & merged02_data1$income == 5 & merged02_data1$education == 2, 1, merged02_data1$employment)

## 2.4.1 - If employment 2 (unemp), income is 4 and education 1 (low) then education should become 2 (medium)
swe_df02_selected1$education <- ifelse(swe_df02_selected1$employment == 2 & swe_df02_selected1$income == 4 & swe_df02_selected1$education == 1, 2, swe_df02_selected1$education)
merged02_data1$education <- ifelse(merged02_data1$employment == 2 & merged02_data1$income == 4 & merged02_data1$education == 1, 2, merged02_data1$education)

## 2.5.1 - If employment 2 (unemp), income is 5 and education 1 (low) then employment should become 1 (emp)
swe_df02_selected1$employment <- ifelse(swe_df02_selected1$employment == 2 & swe_df02_selected1$income == 5 & swe_df02_selected1$education == 1, 1, swe_df02_selected1$employment)
merged02_data1$employment <- ifelse(merged02_data1$employment == 2 & merged02_data1$income == 5 & merged02_data1$education == 1, 1, merged02_data1$employment)

## 2.4.3 - If employment 2 (unemp), income is 4 and education 3 (high) then employment should become 1 (emp)
swe_df02_selected1$employment <- ifelse(swe_df02_selected1$employment == 2 & swe_df02_selected1$income == 4 & swe_df02_selected1$education == 3, 1, swe_df02_selected1$employment)
merged02_data1$employment <- ifelse(merged02_data1$employment == 2 & merged02_data1$income == 4 & merged02_data1$education == 3, 1, merged02_data1$employment)

## 4.5.3 - If employment 1 (emp), income is 5 and education 3 (high) then employment should become 1 (emp)
swe_df02_selected1$employment <- ifelse(swe_df02_selected1$employment == 4 & swe_df02_selected1$income == 5 & swe_df02_selected1$education == 3, 1, swe_df02_selected1$employment)
merged02_data1$employment <- ifelse(merged02_data1$employment == 4 & merged02_data1$income == 5 & merged02_data1$education == 3, 1, merged02_data1$employment)

#Creating synthetic data set - random hot distance deck matching method
group.v <- c("employment","income","education")

rnd.1 <- RANDwNND.hotdeck(data.rec = merged02_data1, data.don = swe_df02_selected1,
                          match.vars = NULL, don.class = group.v)

fA.rnd.1 <- create.fused(data.rec = merged02_data1, data.don = swe_df02_selected1,
                         mtc.ids = rnd.1$mtc.ids, z.vars = c("B3006_1"))

#transforming party categories 96,98,99 to NA
fA.rnd.1 <- fA.rnd.1 %>%
  mutate(B3006_1_modified = ifelse(B3006_1 == 96 | B3006_1 == 98 | B3006_1 == 99, NA, B3006_1))

unique(fA.rnd.1$B3006_1_modified)
glimpse(fA.rnd.1)