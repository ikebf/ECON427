# Set working directory
setwd('C:/Users/lohi/OneDrive - UNC-Wilmington/Documents/R/427')

# Installing packages
install.packages('ipumsr')
install.packages('dplyr')
install.packages('purrr')
install.packages('ggplot2')

# Loading packages
library(ipumsr)
library(dplyr)
library(purrr)
library(ggplot2)

# Save key in .Renviron for use across sessions
set_ipums_api_key("59cba10d8a5da536fc06b59d5068ea8d49f3425282bdb16a9eef8d7d", save = TRUE)

# Note: sample codes are here https://cps.ipums.org/cps-action/samples/sample_ids
?define_extract_cps

# Getting list of variables
namesdf = get_sample_info(collection = 'cps') %>%
              filter(grepl('ASEC', description) & !grepl('Research', description)) %>%
              mutate(year = substr(description, nchar(description) - 4 + 1, nchar(description))) %>% 
              mutate(year = as.numeric(year)) %>%
              filter(year >= 1964) %>%
              pull(name)

# Extracting data from IPUMS
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
df = cps_data %>% filter(AGE >= 21 & AGE <=30 & INCWAGE > 0 & INCWAGE < 99999998) %>%
        filter(EDUC %in% c(70,71,72,73,110,111)) %>%
        filter(ASECWT >= 0)

# Creating variables
df = df %>% mutate(college = ifelse(EDUC %in% c(110, 111), 1, 0)) %>%
      mutate(lwage = log(INCWAGE)) %>%
      mutate(college = as.factor(college))

wage_table = df %>% group_by(YEAR, college) %>%
              summarize(avg_wage = mean(lwage, na.rm = TRUE))

wage_table %>% ggplot(aes(x = YEAR, y = avg_wage, group = college, color = college)) +
  geom_line() +
  ggtitle('Log wages over time') +
  ylab('log(wage)') +
  xlab('year')
  
