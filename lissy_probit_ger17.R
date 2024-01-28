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
selected_vars <- c("E2003", "E2006","E2010","E3013_LH_PL", "E3024_3")
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
                            mtc.ids = rnd.1$mtc.ids, z.vars = c("E3013_LH_PL", "E3024_3"))

# control dimensions of different datasets
dim(data_ger17h)
dim(data_ger17p) #needs to be same size
dim(merged17) #needs to be same size
dim(fused_ger17) #needs to be smaller in size than dim merged17 and ger17p

# Creating vote & partisanship variable --> Right wing = 0 and Left wing = 1
fused_ger17 <- fused_ger17 %>%
  mutate(vote = case_when(
    E3013_LH_PL %in% c(276002, 276005, 276006, 276010, 276011, 276013, 
                                276014, 276015, 276016, 276017, 276021) ~ 1,
    E3013_LH_PL %in% c(276001, 276003, 276004, 276008, 276012, 276019, 
                                276022, 276025, 276026) ~ 0,
    E3013_LH_PL %in% c(999997, 999998, 999999) ~ NA
  ))

fused_ger17 <- fused_ger17 %>%
  mutate(partisanship = case_when(
    E3024_3 %in% c(276002, 276005, 276006, 276010, 276011, 276013, 
                   276014, 276015, 276016, 276017, 276021) ~ 1,
    E3024_3 %in% c(276001, 276003, 276004, 276008, 276012, 276019, 
                   276022, 276025, 276026, 276027) ~ 0,
    E3024_3 %in% c(999997, 999998, 999999) ~ NA
  ))

summary(as.factor(fused_ger17$vote))
summary(as.factor(fused_ger17$partisanship))

# Creating wealth variables
## net wealth
fused_ger17$netwealth <- (fused_ger17$haf + fused_ger17$han) - (fused_ger17$hlr + fused_ger17$hln)
quantiles <- quantile(fused_ger17$netwealth, probs = c(0, 0.2, 0.4, 0.6, 0.8, 1), na.rm = TRUE)
fused_ger17$netwealth_quantile <- cut(fused_ger17$netwealth, breaks = quantiles, labels = FALSE, include.lowest = TRUE)

# wealth
fused_ger17$wealth <- (fused_ger17$haf + fused_ger17$han)
quantiles <- quantile(fused_ger17$wealth, probs = c(0, 0.2, 0.4, 0.6, 0.8, 1), na.rm = TRUE)
fused_ger17$wealth_quantile <- cut(fused_ger17$wealth, breaks = quantiles, labels = FALSE, include.lowest = TRUE)

# financial assets
quantiles <- quantile(fused_ger17$haf, probs = c(0, 0.2, 0.4, 0.6, 0.8, 1), na.rm = TRUE, dig.lab = 5)
fused_ger17$haf_quantile <- findInterval(fused_ger17$haf, quantiles, rightmost.closed = TRUE)

# non-financial assets
quantiles <- quantile(fused_ger17$han, probs = c(0, 0.2, 0.4, 0.6, 0.8, 1), na.rm = TRUE, dig.lab = 5)
fused_ger17$han_quantile <- findInterval(fused_ger17$han, quantiles, rightmost.closed = TRUE)

# business owner
fused_ger17$status1 <- ifelse(fused_ger17$status1 == "[200]self-employed", 1, 0)

# sex
fused_ger17$sex <- ifelse(fused_ger17$sex == "[1]male", 0, 
                          ifelse(fused_ger17$sex == "[2]female", 1, NA))


#Weights
dfs <- c("fused_ger17")

for (i in 1:length(dfs)) {
  tmp <- get(dfs[i])
  mi.idx <- tmp$inum.x
  for (j in 1:5) {
    assign(paste0(dfs[i], "_", j), tmp[mi.idx == j,])
  }
}

#Get the dimensions of each data frame
for (j in 1:5) {
  df_name <- paste0("fused_ger17_", j)
  print(paste("The dimensions of", df_name, "are: ", dim(get(df_name))))
}

#Imputations
ger.mi <- mitools::imputationList(list(fused_ger17_1, fused_ger17_2, fused_ger17_3, fused_ger17_4, fused_ger17_5))

class(ger.mi)

#Creating survey design for weights
ger.svymi <- svydesign(id=~hid.x, 
                       weights=~hpopwgt, 
                       data=ger.mi)

# Regressions
## model 1: net wealth
model_1 <- MIcombine(with(ger.svymi, svyglm(vote ~ 1 + netwealth, family = quasibinomial(link = 'probit'))))

## model 2: wealth
model_2 <- MIcombine(with(ger.svymi, svyglm(vote ~ 1 + wealth_quantile, family = quasibinomial(link = 'probit'))))

## model 3: financial assets
model_3 <- MIcombine(with(ger.svymi, svyglm(vote ~ 1 + haf_quantile, family = quasibinomial(link = 'probit'))))

## model 4: non-financial assets
model_4 <- MIcombine(with(ger.svymi, svyglm(vote ~ 1 + han_quantile, family = quasibinomial(link = 'probit'))))


## model 5: net wealth + age + sex + business owner
model_5 <- MIcombine(with(ger.svymi, svyglm(vote ~ 1 + netwealth_quantile + age + sex + status1, family = quasibinomial(link = 'probit'))))

## model 6: wealth + age + sex + business owner
model_6 <- MIcombine(with(ger.svymi, svyglm(vote ~ 1 + wealth_quantile + age + sex + status1, family = quasibinomial(link = 'probit'))))

## model 7: fin assets + age + sex + business owner
model_7 <- MIcombine(with(ger.svymi, svyglm(vote ~ 1 + haf_quantile + age + sex + status1, family = quasibinomial(link = 'probit'))))

## model 8: non-fin assets + age + sex + business owner
model_8 <- MIcombine(with(ger.svymi, svyglm(vote ~ 1 + han_quantile + age + sex + status1, family = quasibinomial(link = 'probit'))))


## model 9: net wealth + age + sex + business owner + income + partisanship
model_9 <- MIcombine(with(ger.svymi, svyglm(vote ~ 1 + netwealth_quantile + age + sex + status1 + 
                                              income + partisanship, family = quasibinomial(link = 'probit'))))

## model 10: wealth + age + sex + business owner + income + partisanship
model_10 <- MIcombine(with(ger.svymi, svyglm(vote ~ 1 + wealth_quantile + age + sex + status1 + 
                                               income + partisanship, family = quasibinomial(link = 'probit'))))

## model 11: fin assets + age + sex + business owner + income + partisanship
model_11 <- MIcombine(with(ger.svymi, svyglm(vote ~ 1 + haf_quantile + age + sex + status1 + 
                                               income + partisanship, family = quasibinomial(link = 'probit'))))

## model 12: fin assets + age + sex + business owner + income + partisanship
model_12 <- MIcombine(with(ger.svymi, svyglm(vote ~ 1 + han_quantile + age + sex + status1 + 
                                               income + partisanship, family = quasibinomial(link = 'probit'))))


## model 13: partisanship ~ net wealth
model_13 <- MIcombine(with(ger.svymi, svyglm(partisanship ~ 1 + netwealth_quantile, family = quasibinomial(link = 'probit'))))


coef(model_1)
coef(model_5)
coef(model_9)
