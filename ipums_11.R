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
library(labelled) # You may have to install this package using install.packages('labelled')

# Defining extract
print(get_sample_info('usa'), n = 1000)

# NOTE: you may have to register to use IPUMS USA at this link: https://uma.pop.umn.edu/usa/registration
usa_ext_def <- define_extract_usa(
  description = "USA extract",
  samples = c("us1960b", "us1990a", "us2017a", "us2018a", "us2019a"),
  variables = c("AGE", "SEX", "YEAR", "INCWAGE", "WKSWORK2", "EDUC", "IND1990", "PERWT")
)

submitted_extract = submit_extract(usa_ext_def)
downloadable_extract = wait_for_extract(submitted_extract)
data_files = download_extract(downloadable_extract)
usa_data = read_ipums_micro(data_files)

# Cleaning and saving data
df_2 = usa_data %>% filter(AGE >= 21 & AGE <= 64 & INCWAGE > 0 & INCWAGE < 999998
                           & WKSWORK2 > 0 & IND1990 >= 10 & IND1990 <= 932) %>%
        slice_sample(n = 2e5)
save(df_2, file = 'df_2.RData')

# Load data
load('df_2.RData')

df = df_2 %>% mutate(wkswork = case_when(WKSWORK2 == 1 ~ 7, WKSWORK2 == 2 ~ 20,
                                         WKSWORK2 == 3 ~ 33, WKSWORK2 == 4 ~ 43.5,
                                         WKSWORK2 == 5 ~ 48.5, WKSWORK2 == 6 ~ 51)) %>%
  mutate(incwage = case_when(YEAR == 2017 ~ INCWAGE * 1.04, YEAR == 2018 ~ INCWAGE * 1.02,
                             .default = INCWAGE)) %>%
  mutate(year = case_when(YEAR %in% c(2017:2019) ~ 2019, .default = YEAR)) %>%
  mutate(lwearn = log(incwage / wkswork))
  
# Analysis
# Q1
earn_table = df %>% group_by(IND1990, year) %>%
  summarize(mean_earn = weighted.mean(lwearn, PERWT))

# Q2
scatter_table = df %>% group_by(IND1990) %>%
  summarize(earn_1990 = weighted.mean(lwearn, PERWT * ifelse(year == 1990, 1, 0)),
            earn_2019 = weighted.mean(lwearn, PERWT * ifelse(year == 2019, 1, 0))) 
scatter_table %>% ggplot(aes(x = earn_1990, y = earn_2019)) + geom_point() +
  geom_smooth(method = lm) +
  xlab("1990 industry earnings") + ylab("2019 industry earnings")

cor(scatter_table$earn_1990, scatter_table$earn_2019, use = "pairwise")

# Q3
scatter_table = df %>% group_by(IND1990) %>%
  summarize(earn_1960 = weighted.mean(lwearn, PERWT * ifelse(year == 1960, 1, 0)),
            earn_2019 = weighted.mean(lwearn, PERWT * ifelse(year == 2019, 1, 0))) 
scatter_table %>% ggplot(aes(x = earn_1960, y = earn_2019)) + geom_point() +
  geom_smooth(method = lm) +
  xlab("1960 industry earnings") + ylab("2019 industry earnings")

cor(scatter_table$earn_1960, scatter_table$earn_2019, use = "pairwise")
