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
  filter(year >= 1994) %>%
  pull(name)

## Extracting data from IPUMS
cps_extract_request = define_extract_cps(
  description = "1964-2023 CPS Data",
  samples = namesdf,
  variables = c("YEAR", "AGE", "NATIVITY", "INCWAGE", "ASECWT", "SEX", "WKSWORK1")
)

submitted_extract = submit_extract(cps_extract_request)
downloadable_extract = wait_for_extract(submitted_extract)
data_files = download_extract(downloadable_extract)

cps_data = read_ipums_micro(data_files)

## Analyzing data (Exercise 1)
# Data selection
df_1 = cps_data %>% filter(AGE >= 21 & AGE <=64 & INCWAGE > 0 & INCWAGE < 99999998 
                           & WKSWORK1 > 0 & NATIVITY > 0)

# Save the data
save(df_1, file = 'df_1.RData')

# Load data
load('df_1.RData')

df = df_1 %>% mutate(generation = case_when(NATIVITY ==5 ~ 'Immigrant', NATIVITY %in% c(2,3,4) ~ 'Immigrant_parent', 
                                            NATIVITY == 1 ~ 'Native_parents')) %>%
  mutate(lwearn = log(INCWAGE) / WKSWORK1) %>%
  dummy_cols(select_columns = "generation")

# Q1 
df %>% group_by(generation, SEX) %>%
  summarize(avg_wk_earn = weighted.mean(lwearn, ASECWT, na.rm = TRUE))

# Q2
df %>% group_by(SEX) %>%
  summarize(wage_diff_1 = weighted.mean(lwearn, ASECWT * generation_Immigrant, na.rm = TRUE) - 
              weighted.mean(lwearn, ASECWT * generation_Immigrant_parent, na.rm = TRUE), 
            wage_diff_2 = weighted.mean(lwearn, ASECWT * generation_Immigrant_parent, na.rm = TRUE) - 
              weighted.mean(lwearn, ASECWT * generation_Native_parents, na.rm = TRUE))

# Q3 (males)
df %>% group_by(YEAR) %>%
  summarize(wage_first_gen = weighted.mean(lwearn, ASECWT * (2 - SEX) * generation_Immigrant),
            wage_sec_gen = weighted.mean(lwearn, ASECWT * (2 - SEX) * generation_Immigrant_parent)) %>%
  ggplot(aes(YEAR)) +
    geom_line((aes(y = wage_first_gen, color = "First gen men"))) +
    geom_line((aes(y = wage_sec_gen, color = "Second gen men"))) +
    xlab('Year') + ylab('Log weekly earnings')

# Q4 (females) Fill this in 
df %>% group_by(YEAR) %>%
  summarize(wage_first_gen = weighted.mean(lwearn, ASECWT * (SEX - 1) * generation_Immigrant),
            wage_sec_gen = weighted.mean(lwearn, ASECWT * (SEX - 1) * generation_Immigrant_parent)) %>%
  ggplot(aes(YEAR)) +
  geom_line((aes(y = wage_first_gen, color = "First gen women"))) +
  geom_line((aes(y = wage_sec_gen, color = "Second gen women"))) +
  xlab('Year') + ylab('Log weekly earnings')

# Q4 (females) Fill this in 
df %>% group_by(YEAR) %>%
  summarize(wage_native = weighted.mean(lwearn, ASECWT * generation_Native_parents)) %>%
  ggplot(aes(YEAR)) +
    geom_line((aes(y = wage_native, color = "Natives w/ native parents"))) +
    xlab('Year') + ylab('Log weekly earnings')

  