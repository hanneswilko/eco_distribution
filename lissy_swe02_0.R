library(dplyr)
library(tidyr)
library(StatMatch)
library(mitools)
library(survey)
library(srvyr)
library(mice)
library(tidyverse)
library(devtools)

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
      educlev %in% c("[100]low, less than upper secondary") ~ 1,
      educlev %in% c("[110]less than primary", "[111]never attended") ~ 1,
      educlev == "[120]primary" ~ 1,
      educlev == "[130]lower secondary" ~ 2,
      educlev %in% c("[200]medium, upper secondary and post-secondary non-tertiary",
                     "[210]upper secondary") ~ 3,
      educlev == "[220]post-secondary non-tertiary" ~ 3,
      educlev %in% c("[300]high, tertiary", "[310]BA, MA or equivalent, short-cycle tertiary",
                     "[311]short-cycle tertiary") ~ 4,
      educlev == "[312]bachelor or equivalent" ~ 4,
      educlev == "[313]master or equivalent" ~ 5,
      educlev == "[320]doctorate or equivalent" ~ 5,
      TRUE ~ NA
    )
  ) %>%
  select(-educlev) 

swe_df02_selected <- swe_df02_selected %>%
  mutate(education = case_when(
    B2003 == 3 ~ 1, #primary
    B2003 == 4 ~ 2, #lower secondary
    B2003 == 5 ~ 3, #upper secondary
    B2003 == 7 ~ 4, #BA
    B2003 == 8 ~ 5, #>BA
    B2003 == 98 ~ NA
  )) %>%
  select(-B2003)


# Removing missing values
swe_df02_selected <- swe_df02_selected[complete.cases(swe_df02_selected$education), ]
unique(swe_df02_selected$education)

merged02 <- merged02[complete.cases(merged02$education), ]
unique(merged02$education)

#Random distance hot deck
group.v <- c("education")
rnd.1 <- RANDwNND.hotdeck(data.rec = merged02, data.don = swe_df02_selected,
                          match.vars = NULL, don.class = group.v)
fA.rnd.1 <- create.fused(data.rec = merged02, data.don = swe_df02_selected,
                         mtc.ids = rnd.1$mtc.ids, z.vars = c("B3006_1"))
unique(fA.rnd.1$B3006_1)

# Assuming dfs contains the names of your imputed datasets
dfs <- c("fA.rnd.1")

# Read replicate weights
data_swe02r <- data_swe02r %>% replace(is.na(.), 0)

# Create separate data frames for each imputed dataset
for (i in 1:length(dfs)) {
  tmp <- get(dfs[i])
  mi.idx <- tmp$inum.x
  for (j in 1:5) {
    assign(paste0(dfs[i], "_", j), tmp[mi.idx == j,])
  }
}

# Create an imputation list
imputed_data <- imputationList(list(fA.rnd.1_1, fA.rnd.1_2, fA.rnd.1_3, fA.rnd.1_4, fA.rnd.1_5))

str(data_swe02r)
str(imputed_data)


# Create survey design for each imputed dataset
swe.svyrw <- svrepdesign(
  data = imputed_data,
  id = ~hid,
  weights = ~hpopwgt,
  repweights = data_swe02r[, -1],  # Exclude first column with id
  scale = 1,
  rscales = rep(1 / 100),  # If using full set
  type = "other",
  combined.weights = TRUE
)

# Modify imputed data within the survey design object
swe.svyrw$designs <- lapply(swe.svyrw$designs, function(design) {
  design$variables$B3006_1 <- ifelse(design$variables$B3006_1 %in% c(1,2,7), 0,
                                        ifelse(design$variables$B3006_1 %in% c(3,4,5,6), 1, NA))
  return(design)
})

# Fit the probit model on each imputed dataset
model_1 <- lapply(swe.svyrw$designs, function(design) {
  svyglm(B3006_1 ~ 1 + haf, design, family = binomial(link = 'probit'))
})

# Combine results from multiple imputed datasets
combined_results <- MIcombine(model_1)

# Display summary of combined results
summary(combined_results)









#Matching process Sweden 2002 CSES&LWS
#Random distance hot deck
swe_df02 <- swe_df02[complete.cases(swe_df02$edu), ]
unique(swe_df02$edu)
swe02p <- swe02p[complete.cases(swe02p$edu), ]
unique(swe02p$edu)
group.v <- c("edu")
rnd.1 <- RANDwNND.hotdeck(data.rec = swe02p, data.don = swe_df02,
                          match.vars = NULL, don.class = group.v)
fA.rnd.1 <- create.fused(data.rec = swe02p, data.don = swe_df02,
                         mtc.ids = rnd.1$mtc.ids, z.vars = c("vote_LH_PL"))
unique(fA.rnd.1$vote_LH_PL)















