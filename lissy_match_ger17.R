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
data_ger17h <- read.LIS('de17h')
data_ger17p <- read.LIS('de17p')
ger_df17 <- read_csv(paste(USR_DIR,"/hwilko/cses5_ger17.csv",sep=""))

#create new hid variable
data_ger17h$hid_i <- paste0(data_ger17h$hid, "-", data_ger17h$inum)
data_ger17p$hid_i <- paste0(data_ger17p$hid, "-", data_ger17p$inum)

#merge HH and Individual data
merged17 <- merge(data_ger17h, data_ger17p, by = "hid_i", all.x = TRUE)

#create data frame with variables of interest from CSES 
selected_vars <- c("E2003", "E2006","E2010","E3013_LH_PL")
ger_df17_selected <- ger_df17 %>% 
  select(all_of(selected_vars))

#transforming education categories
merged17 <- merged17 %>%
  mutate(
    education = case_when(
      educlev == "[100]low, less than upper secondary" ~ 1,
      educlev == "[130]lower secondary" ~ 1,
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

ger_df17_selected <- ger_df17_selected %>%
  mutate(education = case_when(
    E2003 == 2 ~ 1, 
    E2003 == 3 ~ 1, 
    E2003 == 4 ~ 2, 
    E2003 == 5 ~ 2,
    E2003 == 7 ~ 3, 
    E2003 == 8 ~ 3,
    E2003 == 9 ~ 3, 
    E2003 == 96 ~ NA,
    E2003 == 97 ~ NA,
    E2003 == 99 ~ NA
  )) %>%
  select(-E2003)

#transforming employment status variable
merged17 <- merged17 %>%
  mutate(
    employment = case_when(
      lfs == "[100]employed" ~ 1,
      lfs == "[200]unemployed" ~ 2,
      lfs == "[300]not in labour force" ~ 5,
      lfs == "[310]retired" ~ 3,
      lfs == "[320]in education" ~ 4,
      lfs == "[330]disabled" ~ 5,
      lfs == "[340]homemaker" ~ 5,
      TRUE ~ NA
    )
  ) %>%
  select(-lfs)

ger_df17_selected <- ger_df17_selected %>%
  mutate(employment = case_when(
    E2006 == 1 ~ 1,
    E2006 == 2 ~ 1,
    E2006 == 3 ~ 1,
    E2006 == 5 ~ 2,
    E2006 == 6 ~ 4,
    E2006 == 7 ~ 3,
    E2006 == 8 ~ 5,
    E2006 == 10 ~ 5,
    E2006 == 97 ~ NA,
    E2006 == 97 ~ NA,
    E2006 == 99 ~ NA
  )) %>%
  select(-E2006)

#transforming income variable
# Create quantiles
quantiles <- quantile(merged17$hitotal, probs = c(0, 0.2, 0.4, 0.6, 0.8, 1))
# Create a new variable with quantile labels
merged17$hitotal_quantile <- cut(merged17$hitotal, breaks = quantiles, labels = FALSE, include.lowest = TRUE)

merged17 <- merged17 %>%
  mutate(
    income = case_when(
      hitotal_quantile == 1 ~ 1,
      hitotal_quantile == 2 ~ 2,
      hitotal_quantile == 3 ~ 3,
      hitotal_quantile == 4 ~ 4,
      hitotal_quantile == 5 ~ 5,
      TRUE ~ NA
    )
  ) %>%
  select(-hitotal_quantile)

ger_df17_selected <- ger_df17_selected %>%
  mutate(income = case_when(
    E2010 == 1 ~ 1,
    E2010 == 2 ~ 2,
    E2010 == 3 ~ 3,
    E2010 == 4 ~ 4,
    E2010 == 5 ~ 5,
    E2010 == 7 ~ NA,
    E2010 == 8 ~ NA
  )) %>%
  select(-E2010)

# Removing missing values
# Filter missing values for all selected variables
ger_df17_selected1 <- ger_df17_selected[complete.cases(ger_df17_selected[, c("employment", "income", "education")]), ]
merged17_data1 <- merged17[complete.cases(merged17[, c("employment", "income", "education")]), ]

#The following donation classes in data.don are empty, i.e. there are no donors:
##1st-number = employment, 2nd-number = income, 3rd-number = education
'4.2.1 2.5.1 5.5.1 2.4.2 2.1.3 2.4.3 2.5.3'

## 2.4.3 - If employment 2 (unemp), income is 4 and education 3 (high) then employment should become 1 (emp)
ger_df17_selected1$employment <- ifelse(ger_df17_selected1$employment == 2 & ger_df17_selected1$income == 4 & ger_df17_selected1$education == 3, 1, ger_df17_selected1$employment)
merged17_data1$employment <- ifelse(merged17_data1$employment == 2 & merged17_data1$income == 4 & merged17_data1$education == 3, 1, merged17_data1$employment)

## 2.1.3 - If employment 2 (unemp), income is 1 and education 3 (high) then education should become 1 (low)
ger_df17_selected1$education <- ifelse(ger_df17_selected1$employment == 2 & ger_df17_selected1$income == 1 & ger_df17_selected1$education == 3, 1, ger_df17_selected1$education)
merged17_data1$education <- ifelse(merged17_data1$employment == 2 & merged17_data1$income == 1 & merged17_data1$education == 3, 1, merged17_data1$education)

## 4.2.1 - If employment 4 (in edu), income is 2 and education 1 (low) then education should become 2 (med)
ger_df17_selected1$education <- ifelse(ger_df17_selected1$employment == 4 & ger_df17_selected1$income == 2 & ger_df17_selected1$education == 1, 2, ger_df17_selected1$education)
merged17_data1$education <- ifelse(merged17_data1$employment == 4 & merged17_data1$income == 2 & merged17_data1$education == 1, 2, merged17_data1$education)

## 2.5.1 - If employment 2 (unemp), income is 5 and education 1 (low) then employment should become 1 (emp)
ger_df17_selected1$employment <- ifelse(ger_df17_selected1$employment == 2 & ger_df17_selected1$income == 5 & ger_df17_selected1$education == 1, 1, ger_df17_selected1$employment)
merged17_data1$employment <- ifelse(merged17_data1$employment == 2 & merged17_data1$income == 5 & merged17_data1$education == 1, 1, merged17_data1$employment)

## 2.5.3 - If employment 2 (unemp), income is 5 and education 3 (high) then employment should become 1 (emp)
ger_df17_selected1$employment <- ifelse(ger_df17_selected1$employment == 2 & ger_df17_selected1$income == 5 & ger_df17_selected1$education == 3, 1, ger_df17_selected1$employment)
merged17_data1$employment <- ifelse(merged17_data1$employment == 2 & merged17_data1$income == 5 & merged17_data1$education == 3, 1, merged17_data1$employment)

## 5.5.1 - If employment 5 (not in lf), income is 5 and education 1 (low) then education should become 3 (high)
ger_df17_selected1$education <- ifelse(ger_df17_selected1$employment == 5 & ger_df17_selected1$income == 5 & ger_df17_selected1$education == 1, 3, ger_df17_selected1$education)
merged17_data1$education <- ifelse(merged17_data1$employment == 5 & merged17_data1$income == 5 & merged17_data1$education == 1, 3, merged17_data1$education)

## 2.4.2 - If employment 2 (unemp), income is 4 and education 2 (med) then employment should become 1 (emp)
ger_df17_selected1$employment <- ifelse(ger_df17_selected1$employment == 2 & ger_df17_selected1$income == 4 & ger_df17_selected1$education == 2, 1, ger_df17_selected1$employment)
merged17_data1$employment <- ifelse(merged17_data1$employment == 2 & merged17_data1$income == 4 & merged17_data1$education == 2, 1, merged17_data1$employment)

#Creating synthetic data set - random hot distance deck matching method
group.v <- c("employment","income","education")

rnd.1 <- RANDwNND.hotdeck(data.rec = merged17_data1, data.don = ger_df17_selected1,
                          match.vars = NULL, don.class = group.v)

fused_ger17 <- create.fused(data.rec = merged17_data1, data.don = ger_df17_selected1,
                         mtc.ids = rnd.1$mtc.ids, z.vars = c("E3013_LH_PL"))

# control dimensions of different datasets
dim(data_ger17h)
dim(data_ger17p) #needs to be same size
dim(merged17) #needs to be same size
dim(fused_ger17) #needs to be smaller in size than dim merged17 and ger17p
