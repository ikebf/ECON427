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
  filter(grepl('May 2004', description)) %>%
  mutate(year = substr(description, nchar(description) - 4 + 1, nchar(description))) %>% 
  mutate(year = as.numeric(year)) %>%
  pull(name)

## Extracting data from IPUMS
cps_extract_request = define_extract_cps(
  description = "May 2004 CPS Data",
  samples = namesdf,
  variables = c("AGE", "PAIDHOUR", "EARNWEEK", "HOURWAGE", "UHRSWORK1", "EDUC",
                "OCC1990", "WSREGSHFT", "EARNWT", "WSSUPPWT"
  ) ### COMPLETE THIS LIST WITH THE APPROPRIATE VARIABLES ###
)

submitted_extract = submit_extract(cps_extract_request)
downloadable_extract = wait_for_extract(submitted_extract)
data_files = download_extract(downloadable_extract)

cps_data = read_ipums_micro(data_files)

## Analyzing data (Exercise 1)
# Data selection
df_2 = cps_data %>% filter(AGE >= 16 & AGE <= 64 & PAIDHOUR %in% c(1,2) 
                           & !is.na(EARNWEEK) & !is.na(HOURWAGE) & !is.na(UHRSWORK1) 
                           & UHRSWORK1 > 0 & UHRSWORK1 < 997 
                           & EARNWEEK > 0 & EARNWEEK < 9999.99) ### Fill in the appropriate sample information ###

# Save the data
save(df_2, file = 'df_2.RData')

# Load data
load('df_2.RData')

# Creating variables
df = df_2 %>% mutate(hrwage = ifelse(PAIDHOUR == 2, HOURWAGE, EARNWEEK/UHRSWORK1)) %>%
  mutate(irregular_shift = ifelse(WSREGSHFT == 1, 1, 0)) %>%
  mutate(lhs = ifelse(EDUC <= 60, 1, 0), 1, 0) %>%
  mutate(hs = ifelse(EDUC >= 70 & EDUC <= 73, 1, 0)) %>%
  mutate(lcol = ifelse(EDUC >= 80 & EDUC <= 100, 1, 0)) %>%
  mutate(col = ifelse(EDUC >= 110, 1, 0)) %>%
  mutate(age16to20 = ifelse(AGE >= 16 & AGE <= 20, 1, 0)) %>%
  mutate(age21to30 = ifelse(AGE >= 21 & AGE <= 30, 1, 0)) %>%
  mutate(age31to40 = ifelse(AGE >= 31 & AGE <= 40, 1, 0)) %>%
  mutate(age41to50 = ifelse(AGE >= 41 & AGE <= 50, 1, 0)) %>%
  mutate(age51to64 = ifelse(AGE >= 51, 1, 0)) %>%
  mutate(nurse = ifelse(OCC1990 == 95, 1, 0)) %>%
  mutate(cashier = ifelse(OCC1990 == 276, 1, 0)) %>%
  mutate(cook = ifelse(OCC1990 == 436, 1, 0)) %>%
  mutate(truckdriv = ifelse(OCC1990 == 804, 1, 0)) %>%
  mutate_at(vars(lhs:col, age16to20:age51to64, nurse:truckdriv), ~ ifelse(. == 0, NA, .)) %>%
  gather("educ", "present", c('lhs', 'hs', 'lcol', 'col'), na.rm = TRUE) %>%
  gather("age_group", "present", c('age16to20', 'age21to30', 'age31to40', 'age41to50', 'age51to64'), na.rm = TRUE) %>%
  gather("occupation", "present", c('nurse', 'cashier', 'cook', 'truckdriv'), na.rm = TRUE) 

  
# Figure 1
df %>% group_by(educ) %>%
  summarize(ireg_wrk = mean(irregular_shift, na.rm = TRUE)) %>%
  add_row(educ = "Overall", ireg_wrk = mean(df$irregular_shift, na.rm = TRUE))%>%
  ggplot(aes(x = educ, y = ireg_wrk)) + geom_bar(stat="identity") +
  ggtitle('Irregular work by educational group') +
  ylab('Irregular work percentage') +
  xlab('Educational group') +
  ylim(0,1)

# Figure 2  
df %>% group_by(age_group) %>%
  summarize(ireg_wrk = mean(irregular_shift, na.rm = TRUE)) %>%
  add_row(age_group = "Overall", ireg_wrk = mean(df$irregular_shift, na.rm = TRUE))%>%
  ggplot(aes(x = age_group, y = ireg_wrk)) + geom_bar(stat="identity") +
  ggtitle('Irregular work by age group') +
  ylab('Irregular work percentage') +
  xlab('Age group') +
  ylim(0,1)

# Figure 3  
df %>% group_by(educ, irregular_shift) %>%
  mutate(irregular_shift = as.factor(irregular_shift)) %>%
  summarize(avg_wage = mean(hrwage, na.rm = TRUE)) %>%
  ggplot(aes(x = educ, y = avg_wage, fill = irregular_shift)) +
  geom_bar(position = 'dodge', stat = 'identity') +
  ggtitle('Wage by education and irregular work') +
  ylab('Average wage') +
  xlab('Education/irregular shift?') 
  
# Figure 4  
df %>% group_by(occupation) %>%
  summarize(ireg_wrk = mean(irregular_shift, na.rm = TRUE)) %>%
  add_row(occupation = "Overall", ireg_wrk = mean(df$irregular_shift, na.rm = TRUE))%>%
  ggplot(aes(x = occupation, y = ireg_wrk)) + geom_bar(stat="identity") +
  ggtitle('Irregular work by occupation') +
  ylab('Irregular work percentage') +
  xlab('Occupation') +
  ylim(0,1)

# Figure 5  
df %>% group_by(occupation, irregular_shift) %>%
  mutate(irregular_shift = as.factor(irregular_shift)) %>%
  summarize(avg_wage = mean(hrwage, na.rm = TRUE)) %>%
  ggplot(aes(x = occupation, y = avg_wage, fill = irregular_shift)) +
  geom_bar(position = 'dodge', stat = 'identity') +
  ggtitle('Wage by occupation and irregular work') +
  ylab('Average wage') +
  xlab('Occupation/irregular shift?') 

