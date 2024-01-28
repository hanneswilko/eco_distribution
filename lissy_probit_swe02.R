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
swe_df02 <- read_csv(paste(USR_DIR,"/hwilko/cses2_swe02.csv",sep=""))

#create new hid variable
data_swe02h$hid_i <- paste0(data_swe02h$hid, "-", data_swe02h$inum)
data_swe02p$hid_i <- paste0(data_swe02p$hid, "-", data_swe02p$inum)

#merge HH and Individual data
merged02 <- merge(data_swe02h, data_swe02p, by = "hid_i", all.x = TRUE)

#create data frame with variables of interest from CSES 
selected_vars <- c("B2003", "B2020","B2010", "B3006_1", "B3035")
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
  )

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
# Create quantiles
quantiles <- quantile(merged02$hitotal, probs = c(0, 0.15, 0.35, 0.65, 0.85, 1))
# Create a new variable with quantile labels
merged02$hitotal_quantile <- cut(merged02$hitotal, breaks = quantiles, labels = FALSE, include.lowest = TRUE)

merged02 <- merged02 %>%
  mutate(
    income = case_when(
      hitotal_quantile == 1 ~ 1,
      hitotal_quantile == 2 ~ 2,
      hitotal_quantile == 3 ~ 3,
      hitotal_quantile == 4 ~ 4,
      hitotal_quantile == 5 ~ 5,
      NA ~ TRUE
    )
  ) %>%
  select(-hitotal_quantile)

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
swe_df02_selected1 <- swe_df02_selected[complete.cases(swe_df02_selected[, c("employment", "income", "education")]), ]
merged02_data1 <- merged02[complete.cases(merged02[, c("employment", "income", "education")]), ]

#The following donation classes in data.don are empty, i.e. there are no donors:
##1st-number = employment, 2nd-number = income, 3rd-number = education
'2.4.1 2.5.1 2.4.3 4.5.3 4.5.2'

#Manually cleaning donor classes for matching
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

fused_swe02 <- create.fused(data.rec = merged02_data1, data.don = swe_df02_selected1,
                            mtc.ids = rnd.1$mtc.ids, z.vars = c("B3006_1", "B3035"))

# control dimensions of different datasets
dim(data_swe02h)
dim(data_swe02p) #needs to be same size
dim(merged02) #needs to be same size
dim(fused_swe02) #needs to be smaller in size than dim merged17 and ger17p

summary(as.factor(fused_swe02$B3006_1))
summary(as.factor(fused_swe02$B3035))

# Creating vote & partisanship variable --> Right wing = 0 and Left wing = 1
fused_swe02 <- fused_swe02 %>%
  mutate(vote = case_when(
    B3006_1 %in% c(1, 2, 7, 9) ~ 1,
    B3006_1 %in% c(3, 4, 5, 6, 8, 10) ~ 0,
    B3006_1 %in% c(96, 98, 99) ~ NA
  ))

fused_swe02 <- fused_swe02 %>%
  mutate(partisanship = case_when(
    B3035 %in% c(1, 2, 7, 9) ~ 1,
    B3035 %in% c(3, 4, 5, 6, 10) ~ 0,
    B3035 %in% c(98,99) ~ NA
  ))

fused_swe02 <- fused_swe02 %>%
  mutate(partisanship_factor = case_when(
    B3035 == 98 ~ "Independent",
    B3035 %in% c(1, 2, 7, 9) ~ "Left",
    B3035 %in% c(3, 4, 5, 6, 10) ~ "Right",
    B3035 == 99 ~ NA
  )) %>%
  mutate(partisanship_factor, factor(partisanship_factor, levels = c("Independent", "Left", "Right")))

summary(fused_swe02$partisanship_factor)

# Creating wealth variables
## net wealth
fused_swe02$netwealth <- (fused_swe02$haf + fused_swe02$han) - (fused_swe02$hlr + fused_swe02$hln)
quantiles <- quantile(fused_swe02$netwealth, probs = c(0, 0.2, 0.4, 0.6, 0.8, 1), na.rm = TRUE)
fused_swe02$netwealth_quantile <- cut(fused_swe02$netwealth, breaks = quantiles, labels = FALSE, include.lowest = TRUE)

# wealth
fused_swe02$wealth <- (fused_swe02$haf + fused_swe02$han)
quantiles <- quantile(fused_swe02$wealth, probs = c(0, 0.2, 0.4, 0.6, 0.8, 1), na.rm = TRUE)
fused_swe02$wealth_quantile <- cut(fused_swe02$wealth, breaks = quantiles, labels = FALSE, include.lowest = TRUE)

# financial assets
quantiles <- quantile(fused_swe02$haf, probs = c(0, 0.2, 0.4, 0.6, 0.8, 1), na.rm = TRUE)
fused_swe02$haf_quantile <- cut(fused_swe02$haf, breaks = quantiles, labels = FALSE, include.lowest = TRUE)

# non-financial assets
quantiles <- quantile(fused_swe02$han, probs = c(0, 0.2, 0.4, 0.6, 0.8, 1), na.rm = TRUE, dig.lab = 5)
fused_swe02$han_quantile <- findInterval(fused_swe02$han, quantiles, rightmost.closed = TRUE)

# employment dummy
fused_swe02 <- fused_swe02 %>%
  mutate(
    employment_dummy = ifelse(fused_swe02$lfs == "[100]employed", 1, 0)
  )

summary(as.factor(fused_swe02$employment_dummy))

# business owner
fused_swe02$status1 <- ifelse(fused_swe02$status1 == "[200]self-employed", 1, 0)

# retirement
fused_swe02$retirement <- ifelse(fused_swe02$employment == 3, 1, 0)

# sex
fused_swe02$sex <- ifelse(fused_swe02$sex == "[1]male", 0, 
                         ifelse(fused_swe02$sex == "[2]female", 1, NA))

# Sample weights:
swe.svymi <- svydesign(id = ~ hid.x, 
                       weights = ~ hpopwgt, 
                       data = fused_swe02)

# Regressions
## model 1: net wealth
model_1 <- svyglm(vote ~ 1 + netwealth_quantile, family = quasibinomial(link = 'probit'), design = swe.svymi)

## model 2: wealth
model_2 <- svyglm(vote ~ 1 + wealth_quantile, family = quasibinomial(link = 'probit'), design = swe.svymi)

## model 3: financial assets
model_3 <- svyglm(vote ~ 1 + haf_quantile, family = quasibinomial(link = 'probit'), design = swe.svymi)

## model 4: non-financial assets
model_4 <- svyglm(vote ~ 1 + han_quantile, family = quasibinomial(link = 'probit'), design = swe.svymi)

## model 5: net wealth + income + partisanship + business owner + age + gender + employment + retirement
model_5 <- svyglm(vote ~ 1 + netwealth_quantile + income + status1 + age + sex
                  + employment_dummy, family = quasibinomial(link = 'probit'), design = swe.svymi)

## model 6: wealth + income + partisanship + business owner + age + gender + employment + retirement
model_6 <- svyglm(vote ~ 1 + wealth_quantile + income + status1 + age + sex
                  + employment_dummy + retirement + partisanship_factor, family = quasibinomial(link = 'probit'), design = swe.svymi)

## model 7: fin assets + income + partisanship + business owner + age + gender + employment + retirement
model_7 <- svyglm(vote ~ 1 + haf_quantile + income + status1 + age + sex
                  + employment_dummy + retirement + partisanship_factor, family = quasibinomial(link = 'probit'), design = swe.svymi)

## model 8: non-fin assets + income + partisanship + business owner + age + gender + employment + retirement
model_8 <- svyglm(vote ~ 1 + han_quantile + income + status1 + age + sex
                  + employment + retirement + partisanship_factor, family = quasibinomial(link = 'probit'), design = swe.svymi)

## model 9: partisanship explained by wealth
model_9 <- svyglm(partisanship ~ 1 + netwealth_quantile, family = quasibinomial(link = 'probit'), design = swe.svymi)

summary(model_1)
summary(model_4)
summary(model_5)
summary(model_9)
