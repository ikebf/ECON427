# Clear memory
rm(list = ls())

# Set working directory (customize this step with an appropriate folder)
setwd('C:/Users/lohi/Documents/R')

# Loading packages
library(ipumsr)
library(dplyr)
library(purrr)
library(ggplot2)
library(data.table)
library(tidyr)
library(broom)

# Getting list of variables
namesdf = get_sample_info(collection = 'cps') %>%
  filter(!grepl('ASEC', description)) %>%
  mutate(year = substr(description, nchar(description) - 4 + 1, nchar(description))) %>% 
  mutate(year = as.numeric(year)) %>%
  filter(year >= 1984) %>% ### FILL THIS IN WITH THE APPROPRIATE STARTING YEAR ###
  filter(!(name %in% c('cps2022_02b', 'cps2022_05b'))) %>%
  pull(name)

## Extracting data from IPUMS
cps_extract_request = define_extract_cps(
  description = "1984-2023 CPS Data",
  samples = namesdf,
  variables = c("YEAR", "AGE", "EDUC", "EARNWT", "SEX", "UNION", "EARNWEEK")
)

submitted_extract = submit_extract(cps_extract_request)
downloadable_extract = wait_for_extract(submitted_extract)
data_files = download_extract(downloadable_extract)

cps_data = read_ipums_micro(data_files)

## Analyzing data
# Data selection
df_1 = cps_data %>% filter(AGE >= 21 & AGE <=64 & UNION <= 3 & UNION >= 1 
                           & EARNWEEK > 0 & EARNWEEK < 9999.99) %>%
        slice_sample(n = 1e5)

# Save the data
save(df_1, file = 'df_1.RData')

# Load data
load('df_1.RData')

# Creating variables
df = df_1 %>% mutate(learnwk = log(EARNWEEK)) %>%
  mutate(educ = case_when(EDUC %in% c(000, 001, 002) ~ 0,  EDUC == 10 ~ 2.5, 
                          EDUC == 11 ~ 1, EDUC == 12 ~ 2, EDUC == 13 ~ 3,
                          EDUC == 14 ~ 4, EDUC == 20 ~ 5.5, EDUC == 21 ~ 5,
                          EDUC == 22 ~ 6, EDUC == 30 ~ 7.5, EDUC == 31 ~ 7,
                          EDUC == 32 ~ 8, EDUC == 40 ~ 9, EDUC == 50 ~ 10, 
                          EDUC == 60 ~ 11, EDUC %in% c(70:73) ~ 12,
                          EDUC %in% c(80, 81) ~ 13, EDUC %in% c(90:92) ~ 14,
                          EDUC == 100 ~ 15, EDUC %in% c(110, 111) ~ 16,
                          EDUC %in% c(120, 121) ~ 17, EDUC %in% c(122:123) ~ 18,
                          EDUC == 124 ~ 20, EDUC == 125 ~ 21)) %>%
  mutate(exper = AGE - educ - 6) %>%
  mutate(union = ifelse(UNION == 2, 1, 0)) %>%
  mutate(sex = 2 - SEX) %>%
  mutate(sex_cat = case_when(sex == 0 ~ 'Female', sex == 1 ~ 'Male'))
  
# Q1
df %>% group_by(YEAR) %>%
  summarize(union_frac_male = weighted.mean(union, EARNWT * sex),
          union_frac_fem= weighted.mean(union, EARNWT * (1-sex))) %>%
  ggplot(aes(YEAR)) +
  geom_line((aes(y = union_frac_male, color = "Male union fraction"))) +
  geom_line((aes(y = union_frac_fem, color = "Female union fraction"))) +
  xlab('Year') + ylab('Union fraction') +
  ggtitle('Union fraction by sex')

# Q2 
df %>% group_by(YEAR, sex_cat) %>%
  summarize(union_gap = lm(learnwk ~ educ + exper + I(exper^2) + union)$coefficients['union']) %>%
  ggplot(aes(x = YEAR, y = union_gap, group = sex_cat, color = sex_cat)) +
  geom_line() +
  xlab('Year') + ylab('Returns to union membership') +
  ggtitle('Returns to union membership by sex')
