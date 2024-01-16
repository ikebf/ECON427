# Clear memory
rm(list = ls())

# Set working directory (customize this step with an appropriate folder)
setwd('C:/Users/lohi/Documents/R')

# Installing packages (do this only once)
install.packages('ipumsr')
install.packages('dplyr')
install.packages('purrr')
install.packages('ggplot2')

# Loading packages
library(ipumsr)
library(dplyr)
library(purrr)
library(ggplot2)

# Save key in .Renviron for use across sessions (try to get your own API key here: https://account.ipums.org/api_keys!)
set_ipums_api_key("...", save = TRUE)

# Note: sample codes are here https://cps.ipums.org/cps-action/samples/sample_ids
?define_extract_cps

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
  variables = c("YEAR", "AGE", "EDUC", "INCWAGE", "ASECWT")
)

submitted_extract = submit_extract(cps_extract_request)
downloadable_extract = wait_for_extract(submitted_extract)
data_files = download_extract(downloadable_extract)

cps_data = read_ipums_micro(data_files)

## Analyzing data (Exercise 1)
# Data selection
df_1 = cps_data %>% filter(AGE >= 21 & AGE <=30 & INCWAGE > 0 & INCWAGE < 99999998) %>%
  filter(EDUC %in% c(70,71,72,73,110,111)) %>%
  filter(ASECWT >= 0)

# Save the data
save(df_1, file = 'df_1.RData')

# Load data
load('df_1.RData')

# Creating variables
df = df_1 %>% mutate(college = ifelse(EDUC %in% c(110, 111), 1, 0)) %>%
  mutate(lwage = log(INCWAGE)) 

# Weighted mean of log wages over time
wage_table = df %>% group_by(YEAR, college) %>%
  summarize(avg_wage = weighted.mean(lwage, ASECWT, na.rm = TRUE))

## Plotting the data 
wage_table %>% mutate(college = as.factor(college)) %>%
  ggplot(aes(x = YEAR, y = avg_wage, group = college, color = college)) +
  geom_line() +
  ggtitle('Log wages over time') +
  ylab('log(wage)') +
  xlab('year')

## Calculating and plotting wage differentials 
diff_table = wage_table %>% mutate(diff = avg_wage - lag(avg_wage)) %>%
  filter(college == 1)

diff_table %>% ggplot(aes(x = YEAR, y = diff))  +
  geom_line() +
  ggtitle('Log wage differentials over time') +
  ylab('log(wage) differential') +
  xlab('year')
