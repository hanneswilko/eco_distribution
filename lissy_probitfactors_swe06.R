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
library(stargazer)

#data
data_swe05h <- read.LIS('se05h')
data_swe05p <- read.LIS('se05p')
swe_df06 <- read_csv(paste(USR_DIR,"/hwilko/cses3_swe06.csv",sep=""))

#create new hid variable
data_swe05h$hid_i <- paste0(data_swe05h$hid, "-", data_swe05h$inum)
data_swe05p$hid_i <- paste0(data_swe05p$hid, "-", data_swe05p$inum)

#merge HH and Individual data
merged05 <- merge(data_swe05h, data_swe05p, by = "hid_i", all.x = TRUE)

#create data frame with variables of interest from CSES 
selected_vars <- c("C2003", "C2020","C2010", "C3023_LH_PL", "C3020_3")
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
      hitotal_quantile == 5 ~ 5,
      NA ~ TRUE
    )
  ) %>%
  select(-hitotal_quantile)

swe_df06_selected <- swe_df06_selected %>%
  mutate(income = case_when(
    C2020 == 1 ~ 1,
    C2020 == 2 ~ 2,
    C2020 == 3 ~ 3,
    C2020 == 4 ~ 4,
    C2020 == 5 ~ 5,
    C2020 == 7 ~ NA,
    C2020 == 8 ~ NA,
    C2020 == 9 ~ NA
    
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

fused_swe06 <- create.fused(data.rec = merged05_data1, data.don = swe_df06_selected1,
                            mtc.ids = rnd.1$mtc.ids, z.vars = c("C3023_LH_PL", "C3020_3"))

# control dimensions of different datasets
dim(data_swe05h)
dim(data_swe05p) #needs to be same size
dim(merged05) #needs to be same size
dim(fused_swe06) #needs to be smaller in size than dim merged17 and ger17p


# Creating vote & partisanship variable --> Right wing = 0 and Left wing = 1
fused_swe06 <- fused_swe06 %>%
  mutate(vote = case_when(
    C3023_LH_PL %in% c(1, 2, 7) ~ 1,
    C3023_LH_PL %in% c(3, 4, 5, 6, 8) ~ 0,
    C3023_LH_PL %in% c(90, 92, 97, 98, 99) ~ NA
  ))

fused_swe06 <- fused_swe06 %>%
  mutate(partisanship = case_when(
    C3020_3 %in% c(1, 2, 7) ~ 1,
    C3020_3 %in% c(3, 4, 5, 6) ~ 0,
    C3020_3 %in% c(90, 97, 99) ~ NA
  ))

# Creating wealth variables
## net wealth
fused_swe06$netwealth <- (fused_swe06$haf + fused_swe06$han) - (fused_swe06$hlr + fused_swe06$hln)
quantiles <- quantile(fused_swe06$netwealth, probs = c(0, 0.2, 0.4, 0.6, 0.8, 1), na.rm = TRUE)
fused_swe06$netwealth_quantile <- cut(fused_swe06$netwealth, breaks = quantiles, labels = FALSE, include.lowest = TRUE)

## wealth
fused_swe06$wealth <- (fused_swe06$haf + fused_swe06$han)
quantiles <- quantile(fused_swe06$wealth, probs = c(0, 0.2, 0.4, 0.6, 0.8, 1), na.rm = TRUE)
fused_swe06$wealth_quantile <- cut(fused_swe06$wealth, breaks = quantiles, labels = FALSE, include.lowest = TRUE)

## financial assets
quantiles <- quantile(fused_swe06$haf, probs = c(0, 0.2, 0.4, 0.6, 0.8, 1), na.rm = TRUE)
fused_swe06$haf_quantile <- cut(fused_swe06$haf, breaks = quantiles, labels = FALSE, include.lowest = TRUE)

## non-financial assets
quantiles <- quantile(fused_swe06$han, probs = c(0, 0.2, 0.4, 0.6, 0.8, 1), na.rm = TRUE, dig.lab = 5)
fused_swe06$han_quantile <- findInterval(fused_swe06$han, quantiles, rightmost.closed = TRUE)

##factor transformation
fused_swe06$netwealth_quantile <- factor(fused_swe06$netwealth_quantile, levels = c("1", "2", "3", "4", "5"), 
                                         labels = c("first", "second", "third", "fourth", "fifth"), ordered = TRUE)
fused_swe06$wealth_quantile <- factor(fused_swe06$wealth_quantile, levels = c("1","2","3","4","5"),
                                      labels = c("first", "second", "third", "fourth", "fifth"), ordered = TRUE)
fused_swe06$haf_quantile <- factor(fused_swe06$haf_quantile, levels = c("1","2","3","4","5"),
                                   labels = c("first", "second", "third", "fourth", "fifth"), ordered = TRUE)
fused_swe06$han_quantile <- factor(fused_swe06$han_quantile, levels = c("1","2","3","4","5"),
                                   labels = c("first", "second", "third", "fourth", "fifth"), ordered = TRUE)

#Other controls
# business owner
fused_swe06$status1 <- ifelse(fused_swe06$status1 == "[200]self-employed", 1, 0)

# sex
fused_swe06$sex <- ifelse(fused_swe06$sex == "[1]male", 0, 
                          ifelse(fused_swe06$sex == "[2]female", 1, NA))

# Sample weights:
swe.svymi <- svydesign(id = ~ hid.x, 
                       weights = ~ hpopwgt, 
                       data = fused_swe06)

# Regressions
## model 1: net wealth
model_1 <- svyglm(vote ~ 1 + netwealth_quantile, family = quasibinomial(link = 'probit'), design = swe.svymi)

## model 2: wealth
model_2 <- svyglm(vote ~ 1 + wealth_quantile, family = quasibinomial(link = 'probit'), design = swe.svymi)

## model 3: financial assets
model_3 <- svyglm(vote ~ 1 + haf_quantile, family = quasibinomial(link = 'probit'), design = swe.svymi)

## model 4: non-financial assets
model_4 <- svyglm(vote ~ 1 + han_quantile, family = quasibinomial(link = 'probit'), design = swe.svymi)


## model 5: net wealth + age + sex + business owner
model_5 <- svyglm(vote ~ 1 + netwealth_quantile + age + sex + status1, family = quasibinomial(link = 'probit'), design = swe.svymi)

## model 6: wealth + age + sex + business owner
model_6 <- svyglm(vote ~ 1 + wealth_quantile + age + sex + status1, family = quasibinomial(link = 'probit'), design = swe.svymi)

## model 7: fin assets + age + sex + business owner
model_7 <- svyglm(vote ~ 1 + haf_quantile + age + sex + status1, family = quasibinomial(link = 'probit'), design = swe.svymi)

## model 8: non-fin assets + age + sex + business owner
model_8 <- svyglm(vote ~ 1 + han_quantile + age + sex + status1, family = quasibinomial(link = 'probit'), design = swe.svymi)


## model 9: net wealth + age + sex + business owner + income + partisanship
model_9 <- svyglm(vote ~ 1 + netwealth_quantile + age + sex + status1 + 
                    income + partisanship, family = quasibinomial(link = 'probit'), design = swe.svymi)

## model 10: wealth + age + sex + business owner + income + partisanship
model_10 <- svyglm(vote ~ 1 + wealth_quantile + age + sex + status1 + 
                     income + partisanship, family = quasibinomial(link = 'probit'), design = swe.svymi)

## model 11: fin assets + age + sex + business owner + income + partisanship
model_11 <- svyglm(vote ~ 1 + haf_quantile + age + sex + status1 + 
                     income + partisanship, family = quasibinomial(link = 'probit'), design = swe.svymi)

## model 12: fin assets + age + sex + business owner + income + partisanship
model_12 <- svyglm(vote ~ 1 + han_quantile + age + sex + status1 + 
                     income + partisanship, family = quasibinomial(link = 'probit'), design = swe.svymi)


## model 13: partisanship ~ net wealth
model_13 <- svyglm(partisanship ~ 1 + netwealth_quantile, family = quasibinomial(link = 'logit'), design = swe.svymi)

summary(model_1)
summary(model_5)
summary(model_9)
summary(model_13)


