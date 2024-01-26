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
data_ger12h <- read.LIS('de12h')
data_ger12p <- read.LIS('de12p')
ger_df13 <- read_csv(paste(USR_DIR,"/hwilko/cses4_ger13.csv",sep=""))

#create new hid variable
data_ger12h$hid_i <- paste0(data_ger12h$hid, "-", data_ger12h$inum)
data_ger12p$hid_i <- paste0(data_ger12p$hid, "-", data_ger12p$inum)

#merge HH and Individual data
merged12 <- merge(data_ger12p, data_ger12h, by= "hid_i", all.x = TRUE)

#create data frame with variables of interest from CSES 
selected_vars <- c("D2003", "D2020","D2010", "D3006_LH_PL")
ger_df13_selected <- ger_df13[, selected_vars]


#transforming education categories
merged12 <- merged12 %>%
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

ger_df13_selected <- ger_df13_selected %>%
  mutate(education = case_when(
    D2003 == 3 ~ 1, 
    D2003 == 4 ~ 2, 
    D2003 == 5 ~ 2,
    D2003 == 6 ~ 2, 
    D2003 == 7 ~ 3, 
    D2003 == 8 ~ 3,
    D2003 == 9 ~ 3, 
    D2003 == 96 ~ NA,
    D2003 == 97 ~ NA,
    D2003 == 99 ~ NA
  )) %>%
  select(-D2003)

#transforming employment status variable
merged12 <- merged12 %>%
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

ger_df13_selected <- ger_df13_selected %>%
  mutate(employment = case_when(
    D2010 == 1 ~ 1,
    D2010 == 2 ~ 1,
    D2010 == 5 ~ 2,
    D2010 == 6 ~ 4,
    D2010 == 7 ~ 3,
    D2010 == 8 ~ 5,
    D2010 == 9 ~ 5,
    D2010 == 10 ~ NA,
    D2010 == 97 ~ NA,
    D2010 == 99 ~ NA
  )) %>%
  select(-D2010)

#transforming income variable
# Create quantiles
quantiles <- quantile(merged12$hitotal, probs = c(0, 0.2, 0.4, 0.6, 0.8, 1))
# Create a new variable with quantile labels
merged12$hitotal_quantile <- cut(merged12$hitotal, breaks = quantiles, labels = FALSE, include.lowest = TRUE)

merged12 <- merged12 %>%
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

ger_df13_selected <- ger_df13_selected %>%
  mutate(income = case_when(
    D2020 == 1 ~ 1,
    D2020 == 2 ~ 2,
    D2020 == 3 ~ 3,
    D2020 == 4 ~ 4,
    D2020 == 5 ~ 5,
    D2020 == 7 ~ NA,
    D2020 == 8 ~ NA,
    D2020 == 9 ~ NA,
  )) %>%
  select(-D2020)


# Removing missing values
# Filter missing values for all selected variables
ger_df13_selected1 <- ger_df13_selected[complete.cases(ger_df13_selected[, c("employment", "income", "education")]), ]
merged12_data1 <- merged12[complete.cases(merged12[, c("employment", "income", "education")]), ]

#The following donation classes in data.don are empty, i.e. there are no donors:
##1st-number = employment, 2nd-number = income, 3rd-number = education
'4.2.1 2.3.1 4.3.1 3.4.1 2.5.1 3.5.1 5.1.3 2.2.3 4.2.3 5.2.3 2.3.3 5.4.3 2.5.3'

## 4.2.1 - If employment 4 (in edu), income is 2 and education 1 (low) then education should become 2 (med)
ger_df13_selected1$education <- ifelse(ger_df13_selected1$employment == 4 & ger_df13_selected1$income == 2 & ger_df13_selected1$education == 1, 2, ger_df13_selected1$education)
merged12_data1$education <- ifelse(merged12_data1$employment == 4 & merged12_data1$income == 2 & merged12_data1$education == 1, 2, merged12_data1$education)

## 4.3.1 - If employment 4 (in edu), income is 2 and education 1 (low) then education should become 2 (med)
ger_df13_selected1$education <- ifelse(ger_df13_selected1$employment == 4 & ger_df13_selected1$income == 3 & ger_df13_selected1$education == 1, 2, ger_df13_selected1$education)
merged12_data1$education <- ifelse(merged12_data1$employment == 4 & merged12_data1$income == 3 & merged12_data1$education == 1, 2, merged12_data1$education)

## 2.5.1 - If employment 2 (unemp), income is 5 and education 1 (low) then employment should become 1 (emp)
ger_df13_selected1$employment <- ifelse(ger_df13_selected1$employment == 2 & ger_df13_selected1$income == 5 & ger_df13_selected1$education == 1, 1, ger_df13_selected1$employment)
merged12_data1$employment <- ifelse(merged12_data1$employment == 2 & merged12_data1$income == 5 & merged12_data1$education == 1, 1, merged12_data1$employment)

## 2.2.3 - If employment 2 (unemp), income is 2 and education 3 (high) then education should become 2 (secondary)
ger_df13_selected1$education <- ifelse(ger_df13_selected1$employment == 2 & ger_df13_selected1$income == 2 & ger_df13_selected1$education == 3, 2, ger_df13_selected1$education)
merged12_data1$education <- ifelse(merged12_data1$employment == 2 & merged12_data1$income == 2 & merged12_data1$education == 3, 2, merged12_data1$education)

## 4.2.3 - If employment 4 (in edu), income is 2 and education 3 (high) then education should become 2 (med)
ger_df13_selected1$education <- ifelse(ger_df13_selected1$employment == 4 & ger_df13_selected1$income == 2 & ger_df13_selected1$education == 3, 2, ger_df13_selected1$education)
merged12_data1$education <- ifelse(merged12_data1$employment == 4 & merged12_data1$income == 2 & merged12_data1$education == 3, 2, merged12_data1$education)

## 2.3.3 - If employment 2 (unemp), income is 3 and education 3 (high) then education should become 2 (med)
ger_df13_selected1$education <- ifelse(ger_df13_selected1$employment == 2 & ger_df13_selected1$income == 3 & ger_df13_selected1$education == 3, 2, ger_df13_selected1$education)
merged12_data1$education <- ifelse(merged12_data1$employment == 2 & merged12_data1$income == 3 & merged12_data1$education == 3, 2, merged12_data1$education)

## 2.5.3 - If employment 2 (unemp), income is 5 and education 3 (high) then employment should become 1 (emp)
ger_df13_selected1$employment <- ifelse(ger_df13_selected1$employment == 2 & ger_df13_selected1$income == 5 & ger_df13_selected1$education == 3, 1, ger_df13_selected1$employment)
merged12_data1$employment <- ifelse(merged12_data1$employment == 2 & merged12_data1$income == 5 & merged12_data1$education == 3, 1, merged12_data1$employment)

## 2.3.1 - If employment 2 (unemp), income is 3 and education 1 (low) then education should become 2 (med)
ger_df13_selected1$education <- ifelse(ger_df13_selected1$employment == 2 & ger_df13_selected1$income == 3 & ger_df13_selected1$education == 1, 2, ger_df13_selected1$education)
merged12_data1$education <- ifelse(merged12_data1$employment == 2 & merged12_data1$income == 3 & merged12_data1$education == 1, 2, merged12_data1$education)

## 3.4.1 - If employment 3 (retired), income is 4 and education 1 (low) then education should become 3 (high)
ger_df13_selected1$education <- ifelse(ger_df13_selected1$employment == 3 & ger_df13_selected1$income == 4 & ger_df13_selected1$education == 1, 3, ger_df13_selected1$education)
merged12_data1$education <- ifelse(merged12_data1$employment == 3 & merged12_data1$income == 4 & merged12_data1$education == 1, 3, merged12_data1$education)

## 5.1.3 - If employment 5 (not in lf), income is 1 and education 3 (high) then education should become 2 (med)
ger_df13_selected1$education <- ifelse(ger_df13_selected1$employment == 5 & ger_df13_selected1$income == 1 & ger_df13_selected1$education == 3, 2, ger_df13_selected1$education)
merged12_data1$education <- ifelse(merged12_data1$employment == 5 & merged12_data1$income == 1 & merged12_data1$education == 3, 2, merged12_data1$education)

## 5.2.3 - If employment 5 (not in lf), income is 2 and education 3 (high) then education should become 2 (med)
ger_df13_selected1$education <- ifelse(ger_df13_selected1$employment == 5 & ger_df13_selected1$income == 2 & ger_df13_selected1$education == 3, 2, ger_df13_selected1$education)
merged12_data1$education <- ifelse(merged12_data1$employment == 5 & merged12_data1$income == 2 & merged12_data1$education == 3, 2, merged12_data1$education)

## 5.4.3 - If employment 5 (not in lf), income is 4 and education 3 (high) then employment should become 1 (emp)
ger_df13_selected1$employment <- ifelse(ger_df13_selected1$employment == 5 & ger_df13_selected1$income == 4 & ger_df13_selected1$education == 3, 1, ger_df13_selected1$employment)
merged12_data1$employment <- ifelse(merged12_data1$employment == 5 & merged12_data1$income == 4 & merged12_data1$education == 3, 1, merged12_data1$employment)

## 3.5.1 - If employment 3 (retired), income is 5 and education 1 (low) then education should become 3 (high)
ger_df13_selected1$education <- ifelse(ger_df13_selected1$employment == 3 & ger_df13_selected1$income == 5 & ger_df13_selected1$education == 1, 3, ger_df13_selected1$education)
merged12_data1$education <- ifelse(merged12_data1$employment == 3 & merged12_data1$income == 5 & merged12_data1$education == 1, 3, merged12_data1$education)


#Creating synthetic data set - random hot distance deck matching method
group.v <- c("employment","income","education")

rnd.1 <- RANDwNND.hotdeck(data.rec = merged12_data1, data.don = ger_df13_selected1,
                          match.vars = NULL, don.class = group.v)

fused_ger13 <- create.fused(data.rec = merged12_data1, data.don = ger_df13_selected1,
                         mtc.ids = rnd.1$mtc.ids, z.vars = c("D3006_LH_PL"))

# control dimensions of different datasets
dim(data_ger12h)
dim(data_ger12p) #needs to be same size
dim(merged12) #needs to be same size
dim(fused_ger13) #needs to be smaller in size than dim merged12 and ger12p

# Creating modified vote variable --> Right wing = 0 and Left wing = 1
fused_ger13 <- fused_ger13 %>%
  mutate(D3006_LH_PL_modified =
           ifelse(D3006_LH_PL == 90 | D3006_LH_PL == 97 | D3006_LH_PL == 98 | D3006_LH_PL == 99, NA, D3006_LH_PL))

fused_ger13 <- fused_ger13 %>%
  mutate(vote = case_when(
    D3006_LH_PL_modified == 1 ~ 0, 
    D3006_LH_PL_modified == 4 ~ 1, 
    D3006_LH_PL_modified == 5 ~ 1,
    D3006_LH_PL_modified == 6 ~ 1, 
    D3006_LH_PL_modified == 7 ~ 0, 
    D3006_LH_PL_modified == 8 ~ 0,
    D3006_LH_PL_modified == 9 ~ 1,
    D3006_LH_PL_modified == 10 ~ 0,
    D3006_LH_PL_modified == 11 ~ 0,
    D3006_LH_PL_modified == 12 ~ 1,
    D3006_LH_PL_modified == 13 ~ 1,
    D3006_LH_PL_modified == 14 ~ 0,
    D3006_LH_PL_modified == 17 ~ 0,
    TRUE ~ NA
  ))

# Creating net wealth and net wealth in quantiles
fused_ger13$netwealth <- (fused_ger13$haf + fused_ger13$han) - (fused_ger13$hlr + fused_ger13$hln)

quantiles <- quantile(fused_ger13$netwealth, probs = c(0, 0.2, 0.4, 0.6, 0.8, 1), na.rm = TRUE)
fused_ger13$netwealth_quantile <- cut(fused_ger13$netwealth, breaks = quantiles, labels = FALSE, include.lowest = TRUE)

#Weights
dfs <- c("fused_ger13")

for (i in 1:length(dfs)) {
  tmp <- get(dfs[i])
  mi.idx <- tmp$inum.x
  for (j in 1:5) {
    assign(paste0(dfs[i], "_", j), tmp[mi.idx == j,])
  }
}

# Get the dimensions of each data frame
for (j in 1:5) {
  df_name <- paste0("fused_ger13_", j)
  print(paste("The dimensions of", df_name, "are: ", dim(get(df_name))))
}

ger.mi <- mitools::imputationList(list(fused_ger13_1, fused_ger13_2, fused_ger13_3, fused_ger13_4, fused_ger13_5))

class(ger.mi)

# Creating survey design for weights
ger.svymi <- svydesign(id=~hid.x, 
                    weights=~hpopwgt, 
                    data=ger.mi)

#Regression:
combined_results <- MIcombine(with(ger.svymi, svyglm(vote ~ 1 + netwealth, family = quasibinomial(link = 'probit'))))
coef(combined_results)



























