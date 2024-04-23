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
  mutate(month = substr(description, 12, nchar(description) - 5)) %>%
  mutate(year = as.numeric(year)) %>%
  filter(year %in% c(2007, 2010, 2019, 2020)) %>% ### FILL THIS IN WITH THE APPROPRIATE STARTING YEAR ###
  filter(year != 2020 | month %in% c("April", "May", "June", "July", "August", "September")) %>%
  pull(name)

## Extracting data from IPUMS
cps_extract_request = define_extract_cps(
  description = "COVID CPS Data",
  samples = namesdf,
  variables = c("YEAR", "MONTH", "AGE", "LABFORCE", "EMPSTAT", "EDUC", "WTFINL")
)

submitted_extract = submit_extract(cps_extract_request)
downloadable_extract = wait_for_extract(submitted_extract)
data_files = download_extract(downloadable_extract)

cps_data = read_ipums_micro(data_files)

## Analyzing data
# Data selection
df_1 = cps_data %>% filter(AGE >= 16 & AGE <= 69 & LABFORCE == 2) 

# Save the data
save(df_1, file = 'df_1.RData')

# Load data
load('df_1.RData')

# Creating variables
df = df_1 %>% mutate(unemployed = ifelse(EMPSTAT %in% c(21, 22), 1, 0)) %>%
  mutate(age_grp = case_when(16 <= AGE & AGE <= 19 ~ '16to19',
                             20 <= AGE & AGE <= 29 ~ '20to29',
                             30 <= AGE & AGE <= 39 ~ '30to39',
                             40 <= AGE & AGE <= 49 ~ '40to49',
                             50 <= AGE & AGE <= 59 ~ '50to59',
                             60 <= AGE & AGE <= 69 ~ '60to69')) %>%
  mutate(educ_grp = case_when(EDUC <= 60 ~ 'lessthanHS',
                              70 <= EDUC & EDUC <= 73 ~ 'HS',
                              80 <= EDUC & EDUC <= 100 ~ 'someCol',
                              EDUC == 111 ~ 'Col',
                              EDUC >= 123 ~ 'morethanCol'))

# Q1
df %>% group_by(age_grp) %>%
  summarize(un_2007 = weighted.mean(unemployed * WTFINL * ifelse(YEAR == 2007, 1, 0)),
            un_2010 = weighted.mean(unemployed * WTFINL * ifelse(YEAR == 2010, 1, 0))) %>%
  ggplot(aes(x = un_2007, y = un_2010)) + geom_point() + xlab('2007 unemployment') +
  ylab('2010 unemployment') + ggtitle('Unemployment by age group') +
  geom_text(aes(label = age_grp), hjust = -0.2) +
  geom_abline(intercept = 0, slope = 1, linetype = 'dashed')

# Q2 
df %>% group_by(age_grp) %>%
  summarize(un_2019 = weighted.mean(unemployed * WTFINL * ifelse(YEAR == 2019, 1, 0)),
            un_2020 = weighted.mean(unemployed * WTFINL * ifelse(YEAR == 2020, 1, 0))) %>%
  ggplot(aes(x = un_2019, y = un_2020)) + geom_point() + xlab('2019 unemployment') +
  ylab('2020 unemployment') + ggtitle('Unemployment by age group') +
  geom_text(aes(label = age_grp), hjust = -0.2) +
  geom_abline(intercept = 0, slope = 1, linetype = 'dashed')

# Q3
df %>% group_by(educ_grp) %>%
  summarize(un_2007 = weighted.mean(unemployed * WTFINL * ifelse(YEAR == 2007, 1, 0)),
            un_2010 = weighted.mean(unemployed * WTFINL * ifelse(YEAR == 2010, 1, 0))) %>%
  ggplot(aes(x = un_2007, y = un_2010)) + geom_point() + xlab('2007 unemployment') +
  ylab('2010 unemployment') + ggtitle('Unemployment by age group') +
  geom_text(aes(label = educ_grp), hjust = -0.2) +
  geom_abline(intercept = 0, slope = 1, linetype = 'dashed')

# Q4
df %>% group_by(educ_grp) %>%
  summarize(un_2019 = weighted.mean(unemployed * WTFINL * ifelse(YEAR == 2019, 1, 0)),
            un_2020 = weighted.mean(unemployed * WTFINL * ifelse(YEAR == 2020, 1, 0))) %>%
  ggplot(aes(x = un_2019, y = un_2020)) + geom_point() + xlab('2019 unemployment') +
  ylab('2020 unemployment') + ggtitle('Unemployment by age group') +
  geom_text(aes(label = educ_grp), hjust = -0.2) +
  geom_abline(intercept = 0, slope = 1, linetype = 'dashed')
