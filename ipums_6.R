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
# NOTE: you may have to register to use IPUMS USA at this link: https://uma.pop.umn.edu/usa/registration
usa_ext_def <- define_extract_usa(
  description = "USA extract",
  samples = c("us2019a"),
  variables = c("AGE", "SEX", "YEAR", "DEGFIELD", "DEGFIELDD", "EDUC", "SCHOOL", "INCWAGE", "PERWT")
)

submitted_extract = submit_extract(usa_ext_def)
downloadable_extract = wait_for_extract(submitted_extract)
data_files = download_extract(downloadable_extract)
usa_data = read_ipums_micro(data_files)

# Cleaning and saving data
df_2 = usa_data %>% filter(AGE >= 21 & AGE <= 30 & SCHOOL == 1 & EDUC == 10 & INCWAGE > 0 & INCWAGE < 999998)
save(df_2, file = 'df_2.RData')

# Load data
load('df_2.RData')

df = df_2 %>% mutate(learn = log(INCWAGE)) %>%
  mutate(econ = ifelse(DEGFIELDD == 5501, 1, 0)) %>%
  mutate(deg_labels = labelled::to_factor(DEGFIELD, levels = 'labels'))

deg_counts = df %>% group_by(DEGFIELD) %>%
  summarize(count = n()) %>%
  filter(count >= 300)

# Analysis
# Table reporting average log annual earnings in broad fields of study
learn_table = df %>% group_by(DEGFIELD) %>%
  summarize(avg_learn = weighted.mean(learn, PERWT, na.rm = TRUE)) %>%
  arrange(desc(avg_learn))

# Table reporting gender wage gap for majors
gap_table = df %>% group_by(DEGFIELD) %>%
  summarize(avg_gap = weighted.mean(learn, PERWT * (2 - SEX), na.rm = TRUE) - 
              weighted.mean(learn, PERWT * (SEX - 1), na.rm = TRUE)) %>%
  arrange(desc(avg_gap))

# Scatter diagram showing relationship of avg log hourly earnings of men and 
# percent of workers that are female
scatter_table = df %>% group_by(DEGFIELD) %>%
  summarize(wages_men = weighted.mean(learn, PERWT * (2 - SEX), na.rm = TRUE),
            frac_women = sum((SEX - 1) * PERWT) / sum(PERWT))

scatter_table %>% ggplot(aes(x = wages_men, y = frac_women)) + 
  geom_point() +
  ggtitle('Female participation vs average wage of men') +
  ylab('Average wage of men') +
  xlab('Fraction of major that are women') 

# Breaking down economics/non-econ
# Note: econ saved as 99, non-econ social sciences saved as 98
econ_table = df %>% mutate(DEGFIELD = ifelse(econ == 1, 99, DEGFIELD)) %>%
  mutate(DEGFIELD = ifelse(deg_labels == 'Social Sciences' & econ != 1, 98, DEGFIELD)) %>%
  group_by(DEGFIELD) %>%
  summarize(avg_learn = weighted.mean(learn, PERWT, na.rm = TRUE)) %>%
  arrange(desc(avg_learn))

