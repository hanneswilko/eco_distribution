#packages
library(dplyr)
library(tidyverse)

#data cleaning ---Germany and Sweden---
setwd("/Users/hannes/Documents/Studium/Master/Third_Semester/Economics_of_distribution/seminar/research_project/data/cses")
data_cses3 <- read.csv("cses3.csv")
data_cses4 <- read.csv("cses4.csv")
data_cses5 <- read.csv("cses5.csv")
swe_data2006 <- data_cses3[data_cses3$C1004=="SWE_2006",]
ger_data2013 <- data_cses4[data_cses4$D1004=="DEU_2013",]
ger_data2017 <- data_cses5[data_cses5$E1004=="DEU_2017",]

#Sweden 2006 CSES3
survey_var <- c("C1001","C1002_VER","C1003","C1004","C1005","C1006","C1006_UN","C1006_NAM",
                "C1007","C1008","C1009","C1010_1","C1010_2","C1010_3","C1011_1","C1011_2","C1011_3",
                "C1012_1","C1012_2","C1012_3","C1013","C1014_1","C1014_2","C1014_3","C1015","C1016",
                "C1017","C1018","C1019","C1020","C1021","C1022","C1023","C1024","C1025","C1026","C1027",
                "C1028")
matching_var <- c("C2001","C2002","C2003","C2010","C2020")
expl_var <- c("C2012","C3020_3")
depend_var <- c("C3023_PR_1","C3023_LH_PL", "C3023_UH_PL")

##included variables
swe_df06 <- swe_data2006 %>%
  select(all_of(c(survey_var, matching_var, expl_var, depend_var)))
##renaming
swe_df06 <- swe_df06 %>% 
  rename(age = C2001,
         sex = C2002,
         edu = C2003,
         income = C2020,
         employed = C2010,
         business = C2012,
         vote_LH_PL = C3023_LH_PL,
         vote_UH_PL = C3023_UH_PL,
         vote_PR = C3023_PR_1,
         party_conn = C3020_3)
#merge of vote and party affiliation to left=1, right=0, 89,90,97,99
swe_df06 <- swe_df06 %>%
  mutate(party_conn_clean = case_when(
    party_conn == 1 ~ 1,
    party_conn == 2 ~ 1,
    party_conn == 3 ~ 0,
    party_conn == 4 ~ 0,
    party_conn == 5 ~ 0,
    party_conn == 6 ~ 0,
    party_conn == 7 ~ 1,
    party_conn == 8 ~ 0,
    party_conn == 9 ~ 1,
    party_conn == 10 ~ 0,
    TRUE ~ party_conn  # keep original value for other cases
  ))

swe_df06 <- swe_df06 %>%
  mutate(vote_LH_PL_clean = case_when(
    vote_LH_PL == 1 ~ 1,
    vote_LH_PL == 2 ~ 1,
    vote_LH_PL == 3 ~ 0,
    vote_LH_PL == 4 ~ 0,
    vote_LH_PL == 5 ~ 0,
    vote_LH_PL == 6 ~ 0,
    vote_LH_PL == 7 ~ 1,
    vote_LH_PL == 8 ~ 0,
    vote_LH_PL == 9 ~ 1,
    vote_LH_PL == 10 ~ 0,
    TRUE ~ vote_LH_PL  # keep original value for other cases
  ))

swe_df06 <- swe_df06 %>%
  mutate(vote_UH_PL_clean = case_when(
    vote_UH_PL == 1 ~ 1,
    vote_UH_PL == 2 ~ 1,
    vote_UH_PL == 3 ~ 0,
    vote_UH_PL == 4 ~ 0,
    vote_UH_PL == 5 ~ 0,
    vote_UH_PL == 6 ~ 0,
    vote_UH_PL == 7 ~ 1,
    vote_UH_PL == 8 ~ 0,
    vote_UH_PL == 9 ~ 1,
    vote_UH_PL == 10 ~ 0,
    TRUE ~ vote_UH_PL  # keep original value for other cases
  ))

swe_df06 <- swe_df06 %>%
  mutate(vote_PR_clean = case_when(
    vote_PR == 1 ~ 1,
    vote_PR == 2 ~ 1,
    vote_PR == 3 ~ 0,
    vote_PR == 4 ~ 0,
    vote_PR == 5 ~ 0,
    vote_PR == 6 ~ 0,
    vote_PR == 7 ~ 1,
    vote_PR == 8 ~ 0,
    vote_PR == 9 ~ 1,
    vote_PR == 10 ~ 0,
    TRUE ~ vote_PR  # keep original value for other cases
  ))

#merge of business owner categories: business owner = 1, others = 0, NA=9
swe_df06 <- swe_df06 %>%
  mutate(business_clean = case_when(
    business == 1 ~ 0,
    business == 2 ~ 0,
    business == 3 ~ 0,
    business == 4 ~ 1,
    business == 5 ~ 0,
    business == 6 ~ 0,
    business == 7 ~ 0,
    business == 8 ~ 0,
    TRUE ~ business  # keep original value for other cases
  ))

#merge of employment categories: employed = 1, unemployed = 0, 97,98,99=NA
swe_df06 <- swe_df06 %>%
  mutate(employed_clean = case_when(
    employed == 1 ~ 1,
    employed == 2 ~ 1,
    employed == 3 ~ 1,
    employed == 4 ~ 1,
    employed == 5 ~ 0,
    employed == 6 ~ 0,
    employed == 7 ~ 0,
    employed == 8 ~ 0,
    employed == 9 ~ 0,
    employed == 10 ~ 0,
    TRUE ~ employed  # keep original value for other cases
  ))

#merge of education categories: <=primary = 0, <=secondary = 1, post-secondary = 2, <= undergraduate = 3, 97,98,99=NA
swe_df06 <- swe_df06 %>%
  mutate(edu_clean = case_when(
    edu == 1 ~ 0,
    edu == 2 ~ 0,
    edu == 3 ~ 0,
    edu == 4 ~ 1,
    edu == 5 ~ 1,
    edu == 6 ~ 2,
    edu == 7 ~ 3,
    edu == 8 ~ 3,
    TRUE ~ edu  # keep original value for other cases
  ))

##clean Sweden 2006 data set
swe_df06_clean <- swe_df06 %>%
  select(-c(party_conn,vote_LH_PL,vote_UH_PL,
            vote_PR,business,employed,edu)) %>%
  rename(age_clean = age,
         sex_clean = sex,
         income_clean = income)


#Germany 2013 CSES4
survey_var <- c("D1001","D1002_VER","D1003","D1004","D1005","D1006","D1006_UN","D1006_NAM",
                "D1007","D1008","D1009","D1010_1","D1010_2","D1010_3","D1011_1","D1011_2","D1011_3",
                "D1012_1","D1012_2","D1012_3","D1013","D1014_1","D1014_2","D1014_3","D1015","D1016",
                "D1017","D1018","D1019","D1020","D1021","D1022","D1023","D1024","D1025","D1026","D1027",
                "D1028")
matching_var <- c("D2001_M", "D2001_Y","D2002","D2003","D2010","D2020")
expl_var <- c("D2012","D3018_3")
depend_var <- c("D3006_PR_1","D3006_LH_PL", "D3006_UH_PL")

##included variables
ger_df13 <- ger_data2013 %>%
  select(all_of(c(survey_var, matching_var, expl_var, depend_var)))
##renaming
ger_df13 <- ger_df13 %>% 
  rename(birthM = D2001_M,
         birthY = D2001_Y,
         sex = D2002,
         edu = D2003,
         income = D2020,
         employed = D2010,
         business = D2012,
         vote_LH_PL = D3006_LH_PL,
         vote_UH_PL = D3006_UH_PL,
         vote_PR = D3006_PR_1,
         party_conn = D3018_3)
#merge of vote and party affiliation to left=1,right=0; 89,90,91=other/none; 97,98,99=NA
ger_df13 <- ger_df13 %>%
  mutate(party_conn_clean = case_when(
    party_conn == 1 ~ 0,
    party_conn == 2 ~ 0,
    party_conn == 3 ~ 0,
    party_conn == 4 ~ 1,
    party_conn == 5 ~ 1,
    party_conn == 6 ~ 1,
    party_conn == 7 ~ 0,
    party_conn == 8 ~ 0,
    party_conn == 9 ~ 1,
    party_conn == 10 ~ 0,
    party_conn == 11 ~ 0,
    party_conn == 12 ~ 1,
    party_conn == 13 ~ 1,
    party_conn == 14 ~ 0,
    party_conn == 15 ~ 0,
    party_conn == 16 ~ 0,
    party_conn == 17 ~ 0,
    party_conn == 18 ~ 0,
    party_conn == 19 ~ 1,
    party_conn == 20 ~ 1,
    party_conn == 21 ~ 1,
    TRUE ~ party_conn  # keep original value for other cases
  ))

ger_df13 <- ger_df13 %>% # left=1,right=0; 89-91=other/none; 92,93,97-99=NA
  mutate(vote_LH_PL_clean = case_when(
    vote_LH_PL == 1 ~ 0,
    vote_LH_PL == 2 ~ 0,
    vote_LH_PL == 3 ~ 0,
    vote_LH_PL == 4 ~ 1,
    vote_LH_PL == 5 ~ 1,
    vote_LH_PL == 6 ~ 1,
    vote_LH_PL == 7 ~ 0,
    vote_LH_PL == 8 ~ 0,
    vote_LH_PL == 9 ~ 1,
    vote_LH_PL == 10 ~ 0,
    vote_LH_PL == 11 ~ 0,
    vote_LH_PL == 12 ~ 1,
    vote_LH_PL == 13 ~ 1,
    vote_LH_PL == 14 ~ 0,
    vote_LH_PL == 15 ~ 0,
    vote_LH_PL == 16 ~ 0,
    vote_LH_PL == 17 ~ 0,
    vote_LH_PL == 18 ~ 0,
    vote_LH_PL == 19 ~ 1,
    vote_LH_PL == 20 ~ 1,
    vote_LH_PL == 21 ~ 1,
    TRUE ~ vote_LH_PL  # keep original value for other cases
  ))

ger_df13 <- ger_df13 %>% # left=1,right=0; 89-91=other/none; 92,93,97,98,996,997,999=NA
  mutate(vote_UH_PL_clean = case_when(
    vote_UH_PL == 1 ~ 0,
    vote_UH_PL == 2 ~ 0,
    vote_UH_PL == 3 ~ 0,
    vote_UH_PL == 4 ~ 1,
    vote_UH_PL == 5 ~ 1,
    vote_UH_PL == 6 ~ 1,
    vote_UH_PL == 7 ~ 0,
    vote_UH_PL == 8 ~ 0,
    vote_UH_PL == 9 ~ 1,
    vote_UH_PL == 10 ~ 0,
    vote_UH_PL == 11 ~ 0,
    vote_UH_PL == 12 ~ 1,
    vote_UH_PL == 13 ~ 1,
    vote_UH_PL == 14 ~ 0,
    vote_UH_PL == 15 ~ 0,
    vote_UH_PL == 16 ~ 0,
    vote_UH_PL == 17 ~ 0,
    vote_UH_PL == 18 ~ 0,
    vote_UH_PL == 19 ~ 1,
    vote_UH_PL == 20 ~ 1,
    vote_UH_PL == 21 ~ 1,
    TRUE ~ vote_UH_PL  # keep original value for other cases
  ))

ger_df13 <- ger_df13 %>% # left=1, right=0; 89-91=other/none; 92,93,97,98,99=NA
  mutate(vote_PR_clean = case_when(
    vote_PR == 1 ~ 0,
    vote_PR == 2 ~ 0,
    vote_PR == 3 ~ 0,
    vote_PR == 4 ~ 1,
    vote_PR == 5 ~ 1,
    vote_PR == 6 ~ 1,
    vote_PR == 7 ~ 0,
    vote_PR == 8 ~ 0,
    vote_PR == 9 ~ 1,
    vote_PR == 10 ~ 0,
    vote_PR == 11 ~ 0,
    vote_PR == 12 ~ 1,
    vote_PR == 13 ~ 1,
    vote_PR == 14 ~ 0,
    vote_PR == 15 ~ 0,
    vote_PR == 16 ~ 0,
    vote_PR == 17 ~ 0,
    vote_PR == 18 ~ 0,
    vote_PR == 19 ~ 1,
    vote_PR == 20 ~ 1,
    vote_PR == 21 ~ 1,
    TRUE ~ vote_PR  # keep original value for other cases
  ))

#merge of business owner categories: business owner = 1, others = 0, NA=9
ger_df13 <- ger_df13 %>%
  mutate(business_clean = case_when(
    business == 1 ~ 0,
    business == 2 ~ 0,
    business == 3 ~ 0,
    business == 4 ~ 1,
    business == 5 ~ 0,
    business == 6 ~ 0,
    business == 7 ~ 0,
    business == 8 ~ 0,
    TRUE ~ business  # keep original value for other cases
  ))

#merge of employment categories: employed = 1, unemployed = 0, 97,98,99=NA
ger_df13 <- ger_df13 %>%
  mutate(employed_clean = case_when(
    employed == 1 ~ 1,
    employed == 2 ~ 1,
    employed == 3 ~ 1,
    employed == 4 ~ 1,
    employed == 5 ~ 0,
    employed == 6 ~ 0,
    employed == 7 ~ 0,
    employed == 8 ~ 0,
    employed == 9 ~ 0,
    employed == 10 ~ 0,
    TRUE ~ employed  # keep original value for other cases
  ))

#merge of education categories: <=primary = 0; <=secondary = 1; post-secondary = 2; <= undergraduate = 3; >=graduate = 4; 97,98,99=NA
ger_df13 <- ger_df13 %>%
  mutate(edu_clean = case_when(
    edu == 96 ~ 0,
    edu == 1 ~ 0,
    edu == 2 ~ 0,
    edu == 3 ~ 1,
    edu == 4 ~ 1,
    edu == 5 ~ 2,
    edu == 6 ~ 3,
    edu == 7 ~ 3,
    edu == 8 ~ 4,
    edu == 9 ~ 4,
    TRUE ~ edu  # keep original value for other cases
  ))

#merge of income categories: 1-5=quantiles; 9=NA
ger_df13 <- ger_df13 %>%
  mutate(income_clean = case_when(
    income == 1 ~ 1,
    income == 2 ~ 2,
    income == 3 ~ 3,
    income == 4 ~ 4,
    income == 5 ~ 5,
    income == 7 ~ 9,
    income == 8 ~ 9,
    TRUE ~ income  # keep original value for other cases
  ))

#computing age in years:
ger_df13$age <- ger_df13$D1028 - ger_df13$birthY - (ger_df13$D1026 < ger_df13$birthM)


##clean Germany 2013 data set
ger_df13_clean <- ger_df13 %>%
  select(-c(party_conn,vote_LH_PL,vote_UH_PL,
            vote_PR,business,employed,edu, birthM, birthY, income)) %>%
  rename(sex_clean = sex,
         age_clean = age)


#Germany 2017 CSES5

survey_var <- c("E1001","E1002_VER","E1002_DOI","E1003","E1004","E1005","E1006","E1006_UN",
"E1006_UNALPHA2","E1006_UNALPHA3","E1006_NAM","E1006_REG","E1006_OECD",
"E1006_EU","E1006_VDEM","E1007","E1008","E1009","E1009_P1","E1009_P2",
"E1010_1","E1010_2","E1010_3","E1011_1","E1011_2","E1011_3","E1012_1",
"E1012_2","E1012_3","E1013","E1014_1","E1014_2","E1014_3","E1015",
"E1016","E1017","E1018","E1018_1","E1018_2","E1019","E1020","E1021",
"E1021_1","E1021_2","E1022","E1023","E1024_1","E1024_2","E1024_3",
"E1025_1","E1025_2","E1025_3","E1026","E1027","E1028","E1029","E1030",
"E1031","E1032","E1033","E1034")
matching_var <- c("E2001_A","E2002","E2003","E2010","E2006")
expl_var <- c("E2008","E3024_3")
depend_var <- c("E3013_PR_1","E3013_LH_PL", "E3013_UH_PL")

##included variables
ger_df17 <- ger_data2017 %>%
  select(all_of(c(survey_var, matching_var, expl_var, depend_var)))
##renaming
ger_df17 <- ger_df17 %>% 
  rename(age = E2001_A,
         sex = E2002,
         edu = E2003,
         income = E2010,
         employed = E2006,
         business = E2008,
         vote_LH_PL = E3013_LH_PL,
         vote_UH_PL = E3013_UH_PL,
         vote_PR = E3013_PR_1,
         party_conn = E3024_3)
#merge of vote and party affiliation to left=1,right=0; 999988-93=other/none; 999995-99=NA
ger_df17 <- ger_df17 %>%
  mutate(party_conn_clean = case_when(
    party_conn == 276001 ~ 0,
    party_conn == 276002 ~ 1,
    party_conn == 276003 ~ 0,
    party_conn == 276004 ~ 0,
    party_conn == 276005 ~ 1,
    party_conn == 276006 ~ 1,
    party_conn == 276007 ~ 0,
    party_conn == 276008 ~ 0,
    party_conn == 276010 ~ 1,
    party_conn == 276011 ~ 1,
    party_conn == 276012 ~ 0,
    party_conn == 276013 ~ 1,
    party_conn == 276014 ~ 1,
    party_conn == 276015 ~ 1,
    party_conn == 276016 ~ 1,
    party_conn == 276017 ~ 1,
    party_conn == 276018 ~ 0,
    party_conn == 276019 ~ 0,
    party_conn == 276020 ~ 1,
    party_conn == 276021 ~ 1,
    party_conn == 276022 ~ 0,
    party_conn == 276023 ~ 1,
    party_conn == 276024 ~ 1,
    party_conn == 276025 ~ 0,
    party_conn == 276026 ~ 0,
    party_conn == 276027 ~ 0,
    party_conn == 276028 ~ 0,
    party_conn == 276029 ~ 1,
    TRUE ~ party_conn  # keep original value for other cases
  ))

ger_df17 <- ger_df17 %>% # left=1,right=0; 999988-93=other/none; 999995-99=NA
  mutate(vote_LH_PL_clean = case_when(
    vote_LH_PL == 276001 ~ 0,
    vote_LH_PL == 276002 ~ 1,
    vote_LH_PL == 276003 ~ 0,
    vote_LH_PL == 276004 ~ 0,
    vote_LH_PL == 276005 ~ 1,
    vote_LH_PL == 276006 ~ 1,
    vote_LH_PL == 276007 ~ 0,
    vote_LH_PL == 276008 ~ 0,
    vote_LH_PL == 276010 ~ 1,
    vote_LH_PL == 276011 ~ 1,
    vote_LH_PL == 276012 ~ 0,
    vote_LH_PL == 276013 ~ 1,
    vote_LH_PL == 276014 ~ 1,
    vote_LH_PL == 276015 ~ 1,
    vote_LH_PL == 276016 ~ 1,
    vote_LH_PL == 276017 ~ 1,
    vote_LH_PL == 276018 ~ 0,
    vote_LH_PL == 276019 ~ 0,
    vote_LH_PL == 276020 ~ 1,
    vote_LH_PL == 276021 ~ 1,
    vote_LH_PL == 276022 ~ 0,
    vote_LH_PL == 276023 ~ 1,
    vote_LH_PL == 276024 ~ 1,
    vote_LH_PL == 276025 ~ 0,
    vote_LH_PL == 276026 ~ 0,
    vote_LH_PL == 276027 ~ 0,
    vote_LH_PL == 276028 ~ 0,
    vote_LH_PL == 276029 ~ 1,
    TRUE ~ vote_LH_PL  # keep original value for other cases
  ))

ger_df17 <- ger_df17 %>% # left=1,right=0; 999988-93=other/none; 999995-99=NA
  mutate(vote_UH_PL_clean = case_when(
    vote_UH_PL == 276001 ~ 0,
    vote_UH_PL == 276002 ~ 1,
    vote_UH_PL == 276003 ~ 0,
    vote_UH_PL == 276004 ~ 0,
    vote_UH_PL == 276005 ~ 1,
    vote_UH_PL == 276006 ~ 1,
    vote_UH_PL == 276007 ~ 0,
    vote_UH_PL == 276008 ~ 0,
    vote_UH_PL == 276010 ~ 1,
    vote_UH_PL == 276011 ~ 1,
    vote_UH_PL == 276012 ~ 0,
    vote_UH_PL == 276013 ~ 1,
    vote_UH_PL == 276014 ~ 1,
    vote_UH_PL == 276015 ~ 1,
    vote_UH_PL == 276016 ~ 1,
    vote_UH_PL == 276017 ~ 1,
    vote_UH_PL == 276018 ~ 0,
    vote_UH_PL == 276019 ~ 0,
    vote_UH_PL == 276020 ~ 1,
    vote_UH_PL == 276021 ~ 1,
    vote_UH_PL == 276022 ~ 0,
    vote_UH_PL == 276023 ~ 1,
    vote_UH_PL == 276024 ~ 1,
    vote_UH_PL == 276025 ~ 0,
    vote_UH_PL == 276026 ~ 0,
    vote_UH_PL == 276027 ~ 0,
    vote_UH_PL == 276028 ~ 0,
    vote_UH_PL == 276029 ~ 1,
    TRUE ~ vote_UH_PL  # keep original value for other cases
  ))

ger_df17 <- ger_df17 %>% # left=1,right=0; 999988-93=other/none; 999995-99=NA
  mutate(vote_PR_clean = case_when(
    vote_PR == 276001 ~ 0,
    vote_PR == 276002 ~ 1,
    vote_PR == 276003 ~ 0,
    vote_PR == 276004 ~ 0,
    vote_PR == 276005 ~ 1,
    vote_PR == 276006 ~ 1,
    vote_PR == 276007 ~ 0,
    vote_PR == 276008 ~ 0,
    vote_PR == 276010 ~ 1,
    vote_PR == 276011 ~ 1,
    vote_PR == 276012 ~ 0,
    vote_PR == 276013 ~ 1,
    vote_PR == 276014 ~ 1,
    vote_PR == 276015 ~ 1,
    vote_PR == 276016 ~ 1,
    vote_PR == 276017 ~ 1,
    vote_PR == 276018 ~ 0,
    vote_PR == 276019 ~ 0,
    vote_PR == 276020 ~ 1,
    vote_PR == 276021 ~ 1,
    vote_PR == 276022 ~ 0,
    vote_PR == 276023 ~ 1,
    vote_PR == 276024 ~ 1,
    vote_PR == 276025 ~ 0,
    vote_PR == 276026 ~ 0,
    vote_PR == 276027 ~ 0,
    vote_PR == 276028 ~ 0,
    vote_PR == 276029 ~ 1,
    TRUE ~ vote_PR  # keep original value for other cases
  ))

#merge of business owner categories: business owner = 1, others = 0, NA=9
ger_df17 <- ger_df17 %>%
  mutate(business_clean = case_when(
    business == 1 ~ 0,
    business == 2 ~ 0,
    business == 3 ~ 0,
    business == 4 ~ 1,
    business == 5 ~ 0,
    business == 7 ~ 0,
    business == 8 ~ 0,
    TRUE ~ business  # keep original value for other cases
  ))

#merge of employment categories: employed = 1, unemployed = 0, 97,98,99=NA
ger_df17 <- ger_df17 %>%
  mutate(employed_clean = case_when(
    employed == 1 ~ 1,
    employed == 2 ~ 1,
    employed == 3 ~ 1,
    employed == 4 ~ 1,
    employed == 5 ~ 0,
    employed == 6 ~ 0,
    employed == 7 ~ 0,
    employed == 8 ~ 0,
    employed == 9 ~ 0,
    employed == 10 ~ 0,
    employed == 11 ~ 0,
    employed == 12 ~ 0,
    employed == 13 ~ 0,
    employed == 14 ~ 0,
    TRUE ~ employed  # keep original value for other cases
  ))

#merge of education categories: <=primary = 0; <=secondary = 1; post-secondary = 2; <= undergraduate = 3; >=graduate = 4; 97,98,99=NA
ger_df17 <- ger_df17 %>%
  mutate(edu_clean = case_when(
    edu == 96 ~ 0,
    edu == 1 ~ 0,
    edu == 2 ~ 0,
    edu == 3 ~ 1,
    edu == 4 ~ 1,
    edu == 5 ~ 2,
    edu == 6 ~ 3,
    edu == 7 ~ 3,
    edu == 8 ~ 4,
    edu == 9 ~ 4,
    TRUE ~ edu  # keep original value for other cases
  ))

#merge of income categories: 1-5=quantiles; 7,8,9=NA
ger_df17 <- ger_df17 %>%
  mutate(income_clean = case_when(
    income == 1 ~ 1,
    income == 2 ~ 2,
    income == 3 ~ 3,
    income == 4 ~ 4,
    income == 5 ~ 5,
    TRUE ~ income  # keep original value for other cases
  ))

##clean Germany 2017 data set
ger_df17_clean <- ger_df17 %>%
  select(-c(party_conn,vote_LH_PL,vote_UH_PL,
            vote_PR,business,employed,edu, income)) %>%
  rename(sex_clean = sex,
         age_clean = age)












