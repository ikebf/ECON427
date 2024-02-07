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

# Getting list of variables
namesdf = get_sample_info(collection = 'cps') %>%
  filter(!grepl('ASEC', description)) %>%
  mutate(year = substr(description, nchar(description) - 4 + 1, nchar(description))) %>% 
  mutate(year = as.numeric(year)) %>%
  filter(year == 2021) %>% ### FILL THIS IN WITH THE APPROPRIATE STARTING YEAR ###
  pull(name)

## Extracting data from IPUMS
cps_extract_request = define_extract_cps(
  description = "2021 CPS Data",
  samples = namesdf,
  variables = c("YEAR", "SEX", "AGE", "RACE", "HISPAN", "PAIDHOUR",
                "EARNWEEK", "HOURWAGE", "UHRSWORK1", "EDUC",
                "EARNWT") ### COMPLETE THIS LIST WITH THE APPROPRIATE VARIABLES ###
)

submitted_extract = submit_extract(cps_extract_request)
downloadable_extract = wait_for_extract(submitted_extract)
data_files = download_extract(downloadable_extract)

cps_data = read_ipums_micro(data_files)

## Analyzing data (Exercise 1)
# Data selection
df_2 = cps_data %>% filter(AGE >= 16 & AGE <= 69  & PAIDHOUR %in% c(1,2)) %>%
            filter_at(vars(EARNWEEK, HOURWAGE, UHRSWORK1), all_vars(!is.na(.)))### FILL IN THE AGE RANGES TO CONSIDER ###

# Save the data
save(df_2, file = 'df_2.RData')

# Load data
load('df_2.RData')

# Creating variables
df = df_2 %>% filter(EARNWEEK > 0 & EARNWEEK < 9999.99) %>%
  filter(UHRSWORK1 > 0 & UHRSWORK1 < 997) %>%
  filter(HOURWAGE > 0 & HOURWAGE < 99) %>%
  mutate(wage = ifelse(
    PAIDHOUR == 1, EARNWEEK / UHRSWORK1, HOURWAGE
  )) %>%
  mutate(low_wage = ifelse(
    wage <= 15, 1, 0    
  )) %>%
  mutate(white = ifelse(RACE == 100 & HISPAN == 0, 1, 0)) %>%
  mutate(black = ifelse(RACE == 200 & HISPAN == 0, 1, 0)) %>%
  mutate(asian = ifelse(RACE == 651 & HISPAN == 0, 1, 0)) %>%
  mutate(hispanic = ifelse(HISPAN > 1, 1, 0)) %>%
  mutate(lhs = ifelse(EDUC <= 60, 1, 0), 1, 0) %>%
  mutate(hs = ifelse(EDUC >= 70 & EDUC <= 73, 1, 0)) %>%
  mutate(lcol = ifelse(EDUC >= 80 & EDUC <= 100, 1, 0)) %>%
  mutate(col = ifelse(EDUC >= 110, 1, 0)) %>%
  mutate_at(vars(white:hispanic, lhs:col), ~ ifelse(. == 0, NA, .)) %>%
  gather("race", "present", white:hispanic, na.rm = TRUE) %>%
  gather("educ", "present", lhs:col, na.rm = TRUE)

# Figure 1
df %>% group_by(AGE, SEX) %>%
  summarize(lowwage = mean(low_wage, na.rm = TRUE)) %>% 
  mutate(SEX = as.factor(SEX)) %>%
  ggplot(aes(x = AGE, y = lowwage, group = SEX, color = SEX)) +
  geom_line() +
  ggtitle('Low wage fractions by sex') +
  ylab('Low wage fraction') +
  xlab('Age')

# Figure 2
df %>% filter(race != 5) %>% group_by(AGE, race) %>%
  summarize(lowwage = mean(low_wage, na.rm = TRUE)) %>% 
  ggplot(aes(x = AGE, y = lowwage, group = race, color = race)) +
  geom_line() +
  ggtitle('Low wage fractions by race') +
  ylab('Low wage fraction') +
  xlab('Age')

# Figure 3
df %>% group_by(AGE, educ) %>%
  filter(educ != 1) %>%
  summarize(lowwage = mean(low_wage, na.rm = TRUE)) %>% 
  ggplot(aes(x = AGE, y = lowwage, group = educ, color = educ)) +
  geom_line() +
  ggtitle('Low wage fractions by education') +
  ylab('Low wage fraction') +
  xlab('Age')
