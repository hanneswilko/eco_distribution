library(dplyr)
library(tidyverse)
library(Hmisc)
library(tidyr)
library(haven)
library(acid)
library(StatMatch)
library(mitools)
library(survey)
library(mice)
library(stargazer)
#Load data
data_household13 <- read.LIS('us13h')
data_individual13 <- read.LIS('us13p')
us13wr <- read.LIS('us13r')
weight_us13 <- us13wr %>% replace(is.na(.), 0)
anes12 <- read_dta(paste(USR_DIR, "/stijan/anes_timeseries_2012.dta", sep=""))
print(head(anes12))
#data transformation
data_lws_13 <- merge(data_household13, data_individual13, by.x = c("hid", "inum"), by.y = c("hid", "inum"), all.x = TRUE)
selected_vars <- c("prevote_intpres", "dem_age_r_x","dem_edugroup_x", "dem_empstatus_1digitfin_x", "postvote_presvtwho", "incgroup_prepost_x","libcpo_self")
anes_data12 <- anes12[, selected_vars] 
#clean variables
data_lws_13 <- rename(data_lws_13, education = educlev)
data_lws_13 <- data_lws_13 %>%
  mutate(
    education = case_when(
      education %in% c("[110]less than primary", "[111]never attended", "[120]primary","[130] lower secondary" ,"[100]low, less than upper secondary") ~ 1,
      education %in% c( "[200]medium, upper secondary and post-secondary non-tertiary", "[210]upper secondary", "[220]post-secondary non-tertiary", "[300]high, tertiary") ~ 2,
      education %in% c("[311]short-cycle tertiary", "[310]BA, MA or equivalent, short-cycle tertiary", "[312]bachelor or equivalent") ~ 3,
      education %in% c("[313]master or equivalent", "[320]doctorate or equivalent") ~ 4,
      TRUE ~ NA_integer_
    )
  )
print(unique(data_lws_13$education))
anes_data12 <- rename(anes_data12, education = dem_edugroup_x)
print(unique(anes_data12$education))
anes_data12 <- anes_data12 %>%
  mutate(
    education = case_when(
      education == 1 ~ 1,
      education %in% c(2,3) ~ 2,
      education == 4 ~ 3,
      education == 5 ~ 4,
      TRUE ~ NA_real_
    )
  )
print(unique(anes_data12$education))
#employment
data_lws_13 <- rename(data_lws_13, employment = lfs)
print(unique(data_lws_13$employment))
data_lws_13 <- data_lws_13 %>%
  mutate(
    employment = case_when(
      employment == "[100]employed" ~ 1,
      employment == "[200]unemployed" ~ 2,
      employment %in% c("[300]not in labour force","[340]homemaker") ~ 3,
      employment == "[310]retired" ~ 4,
      employment == "[320]in education" ~ 5,
      employment == "[330]disabled" ~ 6,
      TRUE ~ NA_integer_
    )
  )
print(unique(data_lws_13$employment))
anes_data12 <- rename(anes_data12, employment = dem_empstatus_1digitfin_x)
anes_data12 <- anes_data12 %>%
  mutate(
    employment = case_when(
      employment == 1 ~ 1,
      employment %in% c(2,4) ~ 2,
      employment == 7 ~ 3,
      employment == 5 ~ 4,
      employment == 8 ~ 5,
      employment == 6 ~ 6,
      TRUE ~ NA_integer_
    )
  )
print(unique(anes_data12$employment))
anes_data12$incgroup_prepost_x <- as.factor(anes_data12$incgroup_prepost_x)
names(anes_data12)[names(anes_data12) == "incgroup_prepost_x"] <- "income"
anes_data12 <- anes_data12 %>%
  mutate(
    income = case_when(
      income %in% c("1", "2", "3", "4", "5", "6") ~ "1",
      income %in% c("7", "8", "9", "10", "11", "12") ~ "2",
      income %in% c( "13", "14", "15", "16", "17") ~ "3",
      income %in% c("18", "19", "20", "21", "22") ~ "4",
      income %in% c("23", "24", "25", "26", "27", "28") ~ "5",
      TRUE ~ "NA"
    )
  )
print(unique(anes_data12$income))
data_lws_13 <- rename(data_lws_13, income = hitotal)
data_lws_13 <- data_lws_13 %>%
  mutate(
    income = case_when(
      income < 20599 ~ 1,
      income >= 20600 & income <= 39764 ~ 2,
      income >= 39765 & income <= 64582 ~ 3,
      income >= 64583 & income <= 104096 ~ 4,
      income > 104096 ~ 5,
      TRUE ~ NA_real_
    )
  )
anes12_full <- anes_data12[complete.cases(anes_data12$employment), ]
anes12_full <- anes12_full[complete.cases(anes12_full$income), ]
anes12_full <- anes12_full[complete.cases(anes12_full$education), ]
lws13 <- data_lws_13[complete.cases(data_lws_13$employment), ]
lws13 <- lws13[complete.cases(lws13$income), ]
lws13 <- lws13[complete.cases(lws13$education), ]
#create missing categories
# If employment is 2, income is 2, and education is 1 then education should become 2
anes12_full$education <- ifelse(anes12_full$employment == 6 & anes12_full$income == 2 & anes12_full$education == 4, 3, anes12_full$education)
lws13$education <- ifelse(lws13$employment == 6 & lws13$income == 2 & lws13$education == 4, 3, lws13$education)
group.v <- c("employment", "income","education")
rnd.1 <- RANDwNND.hotdeck(data.rec = lws13, data.don = anes12_full,
                          match.vars = NULL, don.class = group.v)
fused_us12 <- create.fused(data.rec = lws13, data.don = anes12_full,
                           mtc.ids = rnd.1$mtc.ids, z.vars = c("postvote_presvtwho", "libcpo_self"))
unique(fused_us12$postvote_presvtwho)

#wealth transformation
fused_us12$net_wealth <- (fused_us12$haf + fused_us12$han) - (fused_us12$hlr + fused_us12$hln)
fused_us12$net_wealth_quintiles <- cut(fused_us12$net_wealth, breaks = quantile(fused_us12$net_wealth, probs = seq(0, 1, by = 0.2)), include.lowest = TRUE, labels = FALSE)
fused_us12$wealth <- (fused_us12$haf + fused_us12$han)
fused_us12$wealth_quintiles <- cut(fused_us12$wealth, breaks = quantile(fused_us12$wealth, probs = seq(0, 1, by = 0.2)), include.lowest = TRUE, labels = FALSE)
fused_us12$haf_quintiles <- cut(fused_us12$haf, breaks = quantile(fused_us12$haf, probs = seq(0, 1, by = 0.2)), include.lowest = TRUE, labels = FALSE)
fused_us12$han_quintiles <- cut(fused_us12$han, breaks = quantile(fused_us12$han, probs = seq(0, 1, by = 0.2)), include.lowest = TRUE, labels = FALSE)

#factor trasformation
fused_us12$net_wealth_quintiles <- as.factor(fused_us12$net_wealth_quintiles)
fused_us12$net_wealth_quintiles <- relevel(fused_us12$net_wealth_quintiles, ref = "1")

fused_us12$wealth_quintiles <- as.factor(fused_us12$wealth_quintiles)
fused_us12$wealth_quintiles <- relevel(fused_us12$wealth_quintiles, ref = "1")

fused_us12$haf_quintiles <- as.factor(fused_us12$haf_quintiles)
fused_us12$haf_quintiles <- relevel(fused_us12$haf_quintiles, ref = "1")

fused_us12$han_quintiles <- as.factor(fused_us12$han_quintiles)
fused_us12$han_quintiles <- relevel(fused_us12$han_quintiles, ref = "1")

#other regressors transformation
fused_us12$age_group <- cut(fused_us12$age, breaks = c(0, 18, 30, 40, 50, 70,120), labels = c("0-18", "19-30", "31-40", "41-50", "51-70","71-120"))

fused_us12$sex <- ifelse(fused_us12$sex == "[1]male", 0, 
                         ifelse(fused_us12$sex == "[2]female", 1, NA))
print(unique(fused_us12$sex))

fused_us12$status1 <- ifelse(fused_us12$status1 == "[200]self-employed", 1, 0)
print(unique(fused_us12$status1))


fused_us12$employment <- ifelse(fused_us12$employment == 4, 1, 0)
print(unique(fused_us12$employment))

fused_us12$emp <- ifelse(fused_us12$emp == "[1]employed", 1, 
                         ifelse(fused_us12$emp == "[0]not employed", 0, NA))

print(unique(fused_us12$emp))

fused_us12$employed_dummy <- ifelse(fused_us12$employment == 1, 1, 0)
print(unique(fused_us12$employed_dummy))

fused_us12$libcpo_self_recoded <- ifelse(fused_us12$libcpo_self %in% 1:7, fused_us12$libcpo_self, NA)


#weight
dfs <- c("fused_us12")
for (i in 1:length(dfs)) {
  tmp <- get(dfs[i])
  mi.idx <- tmp$inum
  for (j in 1:5) {
    assign(paste0(dfs[i], "_", j), tmp[mi.idx == j,])
  }
}

# Get the dimensions of each data frame
for (j in 1:5) {
  df_name <- paste0("fused_us12_", j)
  print(paste("The dimensions of", df_name, "are: ", dim(get(df_name))))
}

imputed_data <- imputationList(list(fused_us12_1, fused_us12_2, fused_us12_3, fused_us12_4, fused_us12_5))

us.svyrw <- svrepdesign(
  data = imputed_data,
  id = ~hid,
  weights = ~hpopwgt,
  repweights = weight_us13[, -1],  # Exclude first column with id
  scale = 1,
  rscales = rep(1 / 100),  # If using full set
  type = "other",
  combined.weights = TRUE
)

us.svyrw$designs <- lapply(us.svyrw$designs, function(design) {
  design$variables$postvote_presvtwho <- ifelse(design$variables$postvote_presvtwho == 1, 1,
                                                ifelse(design$variables$postvote_presvtwho == 2, 0, NA))
  return(design)
})
# Fit the probit model on each imputed dataset
model_1 <- lapply(us.svyrw$designs, function(design) {
  svyglm(postvote_presvtwho ~ 1 + net_wealth_quintiles, design, family = binomial(link = 'probit'))
})

model_2 <- lapply(us.svyrw$designs, function(design) {
  svyglm(postvote_presvtwho ~ 1 + wealth_quintiles, design, family = binomial(link = 'probit'))
})

model_3 <- lapply(us.svyrw$designs, function(design) {
  svyglm(postvote_presvtwho ~ 1 + haf_quintiles, design, family = binomial(link = 'probit'))
})

model_4 <- lapply(us.svyrw$designs, function(design) {
  svyglm(postvote_presvtwho ~ 1 + han_quintiles, design, family = binomial(link = 'probit'))
})

model_5 <- lapply(us.svyrw$designs, function(design) {
  svyglm(postvote_presvtwho ~ 1 + net_wealth_quintiles + sex + status1 + age, design, family = binomial(link = 'probit'))
})

model_6 <- lapply(us.svyrw$designs, function(design) {
  svyglm(postvote_presvtwho ~ 1 + wealth_quintiles + sex + status1 + age, design, family = binomial(link = 'probit'))
})

model_7 <- lapply(us.svyrw$designs, function(design) {
  svyglm(postvote_presvtwho ~ 1 + haf_quintiles + sex + status1 + age, design, family = binomial(link = 'probit'))
})

model_8 <- lapply(us.svyrw$designs, function(design) {
  svyglm(postvote_presvtwho ~ 1 + han_quintiles + sex + status1 + age, design, family = binomial(link = 'probit'))
})

model_9 <- lapply(us.svyrw$designs, function(design) {
  svyglm(postvote_presvtwho ~ 1 + net_wealth_quintiles + sex + status1 + age + income + 
           as.numeric(libcpo_self_recoded),
         design, family = binomial(link = 'probit'))
})

model_10 <- lapply(us.svyrw$designs, function(design) {
  svyglm(postvote_presvtwho ~ 1 + wealth_quintiles + sex + status1 + age + income + 
           as.numeric(libcpo_self_recoded),
         design, family = binomial(link = 'probit'))
})

model_11 <- lapply(us.svyrw$designs, function(design) {
  svyglm(postvote_presvtwho ~ 1 + haf_quintiles + sex + status1 + age + income + 
           as.numeric(libcpo_self_recoded),
         design, family = binomial(link = 'probit'))
})

model_12 <- lapply(us.svyrw$designs, function(design) {
  svyglm(postvote_presvtwho ~ 1 + han_quintiles + sex + status1 + age + income + 
           as.numeric(libcpo_self_recoded),
         design, family = binomial(link = 'probit'))
})



model_13 <- lapply(us.svyrw$designs, function(design) {
  svyglm(as.ordered(libcpo_self_recoded) ~ 1 + net_wealth_quintiles, design, family = quasibinomial(link = 'logit'))
})

# Combine results from multiple imputed datasets
combined_results_1 <- MIcombine(model_1)
combined_results_2 <- MIcombine(model_2)
combined_results_3 <- MIcombine(model_3)
combined_results_4 <- MIcombine(model_4)
combined_results_5 <- MIcombine(model_5)
combined_results_6 <- MIcombine(model_6)
combined_results_7 <- MIcombine(model_7)
combined_results_8 <- MIcombine(model_8)
combined_results_9 <- MIcombine(model_9)
combined_results_10 <- MIcombine(model_10)
combined_results_11 <- MIcombine(model_11)
combined_results_12 <- MIcombine(model_13)
combined_results_13 <- MIcombine(model_13)

# Display summary of combined results
coef(combined_results_1)
coef(combined_results_2)
coef(combined_results_3)
coef(combined_results_4)
coef(combined_results_5)
coef(combined_results_6)
coef(combined_results_7)
coef(combined_results_8)
coef(combined_results_9)
coef(combined_results_10)
coef(combined_results_11)
coef(combined_results_12)
coef(combined_results_13)


