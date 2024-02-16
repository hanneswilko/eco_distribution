#US16
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
#Load data
data_household16 <- read.LIS('us16h')
data_individual16 <- read.LIS('us16p')
us16wr <- read.LIS('us16r')
weight_us16 <- us16wr %>% replace(is.na(.), 0)

anes16 <- read_dta(paste(USR_DIR, "/stijan/anes_timeseries_2016.dta", sep=""))
print(head(anes16))
#data transformation
data_lws_16 <- merge(data_household16, data_individual16, by.x = "hid", by.y = "hid", all.x = TRUE)
selected_vars <- c("V161109", "V161268", "V161316", "V161324", "V161334", "V162024a", 
                   "V162034a", "V162292a", "V161270", "V161342", "V161267", 
                   "V161277", "V161361x", "V160001", "V161155")
anes_data16 <- anes16[, selected_vars]
#clean variables
#education
data_lws_16 <- rename(data_lws_16, education = educlev)
data_lws_16 <- data_lws_16 %>%
  mutate(
    education = case_when(
      education %in% c("[110]less than primary", "[111]never attended", "[120]primary") ~ 1,
      education %in% c("[130] lower secondary" ,"[100]low, less than upper secondary", "[200]medium, upper secondary and post-secondary non-tertiary", "[210]upper secondary") ~ 2,
      education %in% c("[220]post-secondary non-tertiary", "[300]high, tertiary", "[311]short-cycle tertiary", "[310]BA, MA or equivalent, short-cycle tertiary", "[312]bachelor or equivalent") ~ 3,
      education %in% c("[313]master or equivalent", "[320]doctorate or equivalent") ~ 4,
      TRUE ~ NA_integer_
    )
  )
print(unique(data_lws_16$education))
anes_data16 <- rename(anes_data16, education = V161270)
print(unique(anes_data16$education))
anes_data16 <- anes_data16 %>%
  mutate(
    education = case_when(
      education %in% c(1, 2, 3) ~ 1,
      education %in% c(4, 5, 6, 7, 8, 9, 90) ~ 2,
      education %in% c(10, 11, 12, 13) ~ 3,
      education %in% c(14, 15, 16) ~ 4,
      TRUE ~ NA_real_
    )
  )
print(unique(anes_data16$education))
#employment
data_lws_16 <- rename(data_lws_16, employment = lfs)
print(unique(data_lws_16$employment))
data_lws_16 <- data_lws_16 %>%
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
print(unique(data_lws_16$employment))
anes_data16 <- rename(anes_data16, employment = V161277)
anes_data16 <- anes_data16 %>%
  mutate(
    employment = case_when(
      employment == 1 ~ 1,
      employment %in% c(2,4) ~ 2,
      employment == 7 ~ 3,
      employment == 5 ~ 4,
      employment == 8 ~ 5,
      employment == 6 ~ 6,
      employment %in% c(2, -9) ~ NA_integer_,
      TRUE ~ NA_integer_
    )
  )

print(unique(anes_data16$employment))

anes_data16$V161361x <- as.factor(anes_data16$V161361x)
names(anes_data16)[names(anes_data16) == "V161361x"] <- "income"
anes_data16 <- anes_data16 %>%
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
print(unique(anes_data16$income))


data_lws_16 <- rename(data_lws_16, income = hitotal)


data_lws_16 <- data_lws_16 %>%
  mutate(
    income = case_when(
      income < 24002 ~ 1,
      income >= 24003 & income <= 45600 ~ 2,
      income >= 45601 & income <= 74869 ~ 3,
      income >= 74870 & income <= 121018 ~ 4,
      income > 121019 ~ 5,
      TRUE ~ NA_real_
    )
  )

anes16_full <- anes_data16[complete.cases(anes_data16$employment), ]
anes16_full <- anes16_full[complete.cases(anes16_full$income), ]
anes16_full <- anes16_full[complete.cases(anes16_full$education), ]

lws16 <- data_lws_16[complete.cases(data_lws_16$employment), ]
lws16 <- lws16[complete.cases(lws16$income), ]
lws16 <- lws16[complete.cases(lws16$education), ]


#create missing categories
# If employment is 1, income is 2, and education is 1 then education should become 2
anes16_full$education <- ifelse(anes16_full$employment == 1 & anes16_full$income == 2 & anes16_full$education == 1, 2, anes16_full$education)
lws16$education <- ifelse(lws16$employment == 1 & lws16$income == 2 & lws16$education == 1, 2, lws16$education)

# If employment is 2, income is 2, and education is 1 then education should become 2
anes16_full$education <- ifelse(anes16_full$employment == 2 & anes16_full$income == 2 & anes16_full$education == 1, 2, anes16_full$education)
lws16$education <- ifelse(lws16$employment == 2 & lws16$income == 2 & lws16$education == 1, 2, lws16$education)

# If employment is 5, income is 2, and education is 1 then employment should become 3
anes16_full$employment <- ifelse(anes16_full$employment == 5 & anes16_full$income == 2 & anes16_full$education == 1, 3, anes16_full$employment)
lws16$employment <- ifelse(lws16$employment == 5 & lws16$income == 2 & lws16$education == 1, 3, lws16$employment)

# If employment is 6, income is 2, and education is 1 then employment should become 3
anes16_full$employment <- ifelse(anes16_full$employment == 6 & anes16_full$income == 2 & anes16_full$education == 1, 3, anes16_full$employment)
lws16$employment <- ifelse(lws16$employment == 6 & lws16$income == 2 & lws16$education == 1, 3, lws16$employment)

# If employment is 3, income is 3, and education is 1 then education should become 2
anes16_full$education <- ifelse(anes16_full$employment == 3 & anes16_full$income == 3 & anes16_full$education == 1, 2, anes16_full$education)
lws16$education <- ifelse(lws16$employment == 3 & lws16$income == 3 & lws16$education == 1, 2, lws16$education)

# If employment is 5, income is 3, and education is 1 then education should become 2
anes16_full$education <- ifelse(anes16_full$employment == 5 & anes16_full$income == 3 & anes16_full$education == 1, 2, anes16_full$education)
lws16$education <- ifelse(lws16$employment == 5 & lws16$income == 3 & lws16$education == 1, 2, lws16$education)
# If employment is 6, income is 3, and education is 1 then education should become 2
anes16_full$education <- ifelse(anes16_full$employment == 6 & anes16_full$income == 3 & anes16_full$education == 1, 2, anes16_full$education)
lws16$education <- ifelse(lws16$employment == 6 & lws16$income == 3 & lws16$education == 1, 2, lws16$education)

# If employment is 1, income is 4, and education is 1 then education should become 2
anes16_full$education <- ifelse(anes16_full$employment == 1 & anes16_full$income == 4 & anes16_full$education == 1, 2, anes16_full$education)
lws16$education <- ifelse(lws16$employment == 1 & lws16$income == 4 & lws16$education == 1, 2, lws16$education)

# If employment is 3, income is 4, and education is 1 then education should become 2
anes16_full$education <- ifelse(anes16_full$employment == 3 & anes16_full$income == 4 & anes16_full$education == 1, 2, anes16_full$education)
lws16$education <- ifelse(lws16$employment == 3 & lws16$income == 4 & lws16$education == 1, 2, lws16$education)

# If employment is 6, income is 4, and education is 1 then education should become 2
anes16_full$education <- ifelse(anes16_full$employment == 6 & anes16_full$income == 4 & anes16_full$education == 1, 2, anes16_full$education)
lws16$education <- ifelse(lws16$employment == 6 & lws16$income == 4 & lws16$education == 1, 2, lws16$education)

# If employment is 1, income is 5, and education is 1 then education should become 2
anes16_full$education <- ifelse(anes16_full$employment == 1 & anes16_full$income == 5 & anes16_full$education == 1, 2, anes16_full$education)
lws16$education <- ifelse(lws16$employment == 1 & lws16$income == 5 & lws16$education == 1, 2, lws16$education)

# If employment is 3, income is 5, and education is 1 then education should become 2
anes16_full$education <- ifelse(anes16_full$employment == 3 & anes16_full$income == 5 & anes16_full$education == 1, 2, anes16_full$education)
lws16$education <- ifelse(lws16$employment == 3 & lws16$income == 5 & lws16$education == 1, 2, lws16$education)

# If employment is 6, income is 5, and education is 2 then employment should become 3
anes16_full$employment <- ifelse(anes16_full$employment == 6 & anes16_full$income == 5 & anes16_full$education == 2, 3, anes16_full$employment)
lws16$employment <- ifelse(lws16$employment == 6 & lws16$income == 5 & lws16$education == 2, 3, lws16$employment)

# If employment is 3, income is 1, and education is 4 then education should become 3
anes16_full$education <- ifelse(anes16_full$employment == 3 & anes16_full$income == 1 & anes16_full$education == 4, 3, anes16_full$education)
lws16$education <- ifelse(lws16$employment == 3 & lws16$income == 1 & lws16$education == 4, 3, lws16$education)

# If employment is 5, income is 2, and education is 4 then education should become 3
anes16_full$education <- ifelse(anes16_full$employment == 5 & anes16_full$income == 2 & anes16_full$education == 4, 3, anes16_full$education)
lws16$education <- ifelse(lws16$employment == 5 & lws16$income == 2 & lws16$education == 4, 3, lws16$education)

# If employment is 5, income is 4, and education is 4 then education should become 3
anes16_full$education <- ifelse(anes16_full$employment == 5 & anes16_full$income == 4 & anes16_full$education == 4, 3, anes16_full$education)
lws16$education <- ifelse(lws16$employment == 5 & lws16$income == 4 & lws16$education == 4, 3, lws16$education)

#matching

group.v <- c("employment", "income","education")
rnd.1 <- RANDwNND.hotdeck(data.rec = lws16, data.don = anes16_full,
                          match.vars = NULL, don.class = group.v)

fused_us16 <- create.fused(data.rec = lws16, data.don = anes16_full,
                           mtc.ids = rnd.1$mtc.ids, z.vars = c("V162034a"))
unique(fused_us16$V162034a)

# Print the size of the dataset
print(paste("The dimensions of the dataset are: ", dim(fused_us16)))

fused_us16$net_wealth <- (fused_us16$haf + fused_us16$han) - (fused_us16$hlr + fused_us16$hln)

fused_us16$quintiles <- cut(fused_us16$net_wealth, breaks = quantile(fused_us16$net_wealth, probs = seq(0, 1, by = 0.2)), include.lowest = TRUE, labels = FALSE)

#weight
dfs <- c("fused_us16")

for (i in 1:length(dfs)) {
  tmp <- get(dfs[i])
  mi.idx <- tmp$inum.x
  for (j in 1:5) {
    assign(paste0(dfs[i], "_", j), tmp[mi.idx == j,])
  }
}


# Get the dimensions of each data frame
for (j in 1:5) {
  df_name <- paste0("fused_us16_", j)
  print(paste("The dimensions of", df_name, "are: ", dim(get(df_name))))
}

imputed_data <- imputationList(list(fused_us16_1, fused_us16_2, fused_us16_3, fused_us16_4, fused_us16_5))

us.svyrw <- svrepdesign(
  data = imputed_data,
  id = ~hid,
  weights = ~hpopwgt,
  repweights = weight_us16[, -1],  # Exclude first column with id
  scale = 1,
  rscales = rep(1 / 100),  # If using full set
  type = "other",
  combined.weights = TRUE
)

us.svyrw$designs <- lapply(us.svyrw$designs, function(design) {
  design$variables$V162034a <- ifelse(design$variables$V162034a == 1, 1,
                                      ifelse(design$variables$V162034a == 2, 0, NA))
  return(design)
})


# Fit the probit model on each imputed dataset
model_1 <- lapply(us.svyrw$designs, function(design) {
  svyglm(V162034a ~ 1 + net_wealth, design, family = binomial(link = 'probit'))
})

# Combine results from multiple imputed datasets
combined_results <- MIcombine(model_1)

# Display summary of combined results
coef(combined_results)
