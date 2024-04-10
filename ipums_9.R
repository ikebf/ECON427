# Clear memory
rm(list = ls())

# Set working directory (customize this step with an appropriate folder)
setwd('C:/Users/lohi/Documents/R')

# Loading packages
library(ipumsr)
library(dplyr)
library(purrr)
library(ggplot2)
library(fastDummies)

# Getting list of variables
namesdf = get_sample_info(collection = 'cps') %>%
  filter(grepl('ASEC', description) & !grepl('Research', description)) %>%
  mutate(year = substr(description, nchar(description) - 4 + 1, nchar(description))) %>% 
  mutate(year = as.numeric(year)) %>%
  filter(year >= 1964) %>%
  pull(name)

## Extracting data from IPUMS
cps_extract_request = define_extract_cps(
  description = "1964-2023 CPS Data",
  samples = namesdf,
  variables = c("YEAR", "AGE", "RACE", "EDUC", "ASECWT", "SEX", "WKSWORK2", "INCWAGE")
)

submitted_extract = submit_extract(cps_extract_request)
downloadable_extract = wait_for_extract(submitted_extract)
data_files = download_extract(downloadable_extract)

cps_data = read_ipums_micro(data_files)

## Analyzing data (Exercise 1)
# Data selection
df_1 = cps_data %>% filter(AGE >= 21 & AGE <=64 & INCWAGE > 0 & INCWAGE < 99999998 
                           & WKSWORK2 > 1 & WKSWORK2 <= 6 & RACE %in% c(100,200))

# Save the data
save(df_1, file = 'df_1.RData')

# Load data
load('df_1.RData')

#### Look up variable codes here to decide on the appropriate values of each codebook value below
#### for WKSWORK2: https://usa.ipums.org/usa-action/variables/WKSWORK2#description_section
df = df_1 %>% mutate(race = case_when(RACE == 100 ~ 0, RACE == 200 ~ 1)) %>%
  mutate(more_than_hs = ifelse(EDUC >= 80, 1, 0)) %>%
  mutate(wkswork = case_when(WKSWORK2 == 1 ~ ..., WKSWORK2 == 2 ~ ..., WKSWORK2 == 3 ~ ..., 
                             WKSWORK2 == 4 ~ ..., WKSWORK2 == 5 ~ ..., WKSWORK2 == 6 ~ ...)) %>%
  mutate(lwkearn = log(INCWAGE / wkswork)) %>%
  mutate(sex = case_when(SEX == 1 ~ 'Male', SEX == 2 ~ 'FEMALE')) %>%
  mutate(SEX = 2 - SEX) 

# Q1 
df %>% group_by(YEAR) %>%
  summarize(wage_gap_women = -weighted.mean(lwkearn, ASECWT * (1 - race) * (1 - SEX)) + 
              weighted.mean(lwkearn, ASECWT * race * (1 - SEX)),
            wage_gap_men = -weighted.mean(lwkearn, ASECWT * (1 - race) * SEX) + 
              weighted.mean(lwkearn, ASECWT * race * SEX)) %>%
  ggplot(aes(YEAR)) +
  geom_line((aes(y = wage_gap_women, color = "Female black-white wage gap"))) +
  geom_line((aes(y = wage_gap_men, color = "Male black-white wage gap"))) +
  xlab('Year') + ylab('Log weekly earnings') + 
  ggtitle("log wage gaps by sex")

# Q2
df %>% group_by(YEAR) %>%
  summarize(wage_gap_hs = -weighted.mean(lwkearn, ASECWT * (1 - race) * SEX * (1 - more_than_hs)) + 
              weighted.mean(lwkearn, ASECWT * race * SEX * (1 - more_than_hs)),
            wage_gap_col = -weighted.mean(lwkearn, ASECWT * (1 - race) * SEX * more_than_hs) + 
              weighted.mean(lwkearn, ASECWT * race * SEX * more_than_hs)) %>%
  ggplot(aes(YEAR)) +
  geom_line((aes(y = wage_gap_hs, color = "Less than hs black-white wage gap"))) +
  geom_line((aes(y = wage_gap_col, color = "More than hs black-white wage gap"))) +
  xlab('Year') + ylab('Log weekly earnings') +
  ggtitle('Male log wage gaps by education')

# Q3 
df %>% group_by(YEAR) %>%
  summarize(wage_gap_hs = -weighted.mean(lwkearn, ASECWT * (1 - race) * (1-SEX) * (1 - more_than_hs)) + 
              weighted.mean(lwkearn, ASECWT * race * (1-SEX) * (1 - more_than_hs)),
            wage_gap_col = -weighted.mean(lwkearn, ASECWT * (1 - race) * (1-SEX) * more_than_hs) + 
              weighted.mean(lwkearn, ASECWT * race * (1-SEX) * more_than_hs)) %>%
  ggplot(aes(YEAR)) +
  geom_line((aes(y = wage_gap_hs, color = "Less than hs black-white wage gap"))) +
  geom_line((aes(y = wage_gap_col, color = "More than hs black-white wage gap"))) +
  xlab('Year') + ylab('Log weekly earnings') +
  ggtitle('Female log wage gaps by education')
