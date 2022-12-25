library(readr)
library(dplyr)
library(tidyr)
library(tidyverse)
library(ggplot2)

#Import dataset
DataComplete <- read_csv("DataComplete.csv")
DataComplete

#------------------------------- Distribution -----------------------------------------
#CODE_GENDER
DataComplete %>%
  group_by(CODE_GENDER) %>%
  summarise(n = n()) %>%
  ggplot(aes(CODE_GENDER, n, fill = CODE_GENDER)) + geom_col() + ggtitle("CODE_GENDER", subtitle = "Gender")

#FLAG_OWN_CAR
DataComplete %>%
  group_by(FLAG_OWN_CAR) %>%
  summarise(n = n()) %>%
  ggplot(aes(FLAG_OWN_CAR, n, fill = FLAG_OWN_CAR)) + geom_col() + ggtitle("FLAG_OWN_CAR", subtitle = "Is there a car")

#FLAG_OWN_REALTY
DataComplete %>%
  group_by(FLAG_OWN_REALTY) %>%
  summarise(n = n()) %>%
  ggplot(aes(FLAG_OWN_REALTY, n, fill = FLAG_OWN_REALTY)) + geom_col() + ggtitle("FLAG_OWN_REALTY", subtitle = "Is there a property")

#CNT_CHILDREN
DataComplete %>%
  group_by(CNT_CHILDREN) %>%
  ggplot(aes(CNT_CHILDREN)) + geom_histogram(binwidth = 0.5) + xlim(0,6) + ylim(0,10000) + ggtitle("CNT_CHILDREN", subtitle = "Number of children")

#AMT_INCOME_TOTAL
DataComplete %>%
  group_by(AMT_INCOME_TOTAL) %>%
  ggplot(aes(AMT_INCOME_TOTAL)) + geom_histogram(bins = 20) + xlim(0,500000) + ggtitle("AMT_INCOME_TOTAL", subtitle = "Annual income")

#NAME_INCOME_TYPE
DataComplete %>%
  group_by(NAME_INCOME_TYPE) %>%
  summarise(n = n()) %>%
  ggplot(aes(n, NAME_INCOME_TYPE, fill = NAME_INCOME_TYPE)) + geom_col() + ggtitle("NAME_INCOME_TYPE", subtitle = "Income category")

#NAME_EDUCATION_TYPE
DataComplete %>%
  group_by(NAME_EDUCATION_TYPE) %>%
  summarise(n = n()) %>%
  ggplot(aes(NAME_EDUCATION_TYPE, n, fill = NAME_EDUCATION_TYPE)) + geom_col() + ggtitle("NAME_EDUCATION_TYPE", subtitle = "Education level")

#NAME_FAMILY_STATUS
DataComplete %>%
  group_by(NAME_FAMILY_STATUS) %>%
  summarise(n = n()) %>%
  ggplot(aes(NAME_FAMILY_STATUS, n, fill = NAME_FAMILY_STATUS)) + geom_col() + ggtitle("NAME_FAMILY_STATUS", subtitle = "Marital status")

#NAME_HOUSING_TYPE
DataComplete %>%
  group_by(NAME_HOUSING_TYPE) %>%
  summarise(n = n()) %>%
  ggplot(aes(n, NAME_HOUSING_TYPE, fill = NAME_HOUSING_TYPE)) + geom_col() + ggtitle("NAME_HOUSING_TYPE", subtitle = "Way of living")

#FLAG_WORK_PHONE
DataComplete %>%
  group_by(FLAG_WORK_PHONE) %>%
  ggplot(aes(FLAG_WORK_PHONE)) + geom_histogram(binwidth = 0.5) + ylim(0, 30000) + ggtitle("FLAG_WORK_PHONE", subtitle = "Is there a work phone")

#FLAG_PHONE
DataComplete %>%
  group_by(FLAG_PHONE) %>%
  ggplot(aes(FLAG_PHONE)) + geom_histogram(binwidth = 0.5) + ylim(0, 30000) + ggtitle("FLAG_PHONE", subtitle = "Is there a phone")

#OCCUPATION_TYPE
DataComplete %>%
  group_by(OCCUPATION_TYPE) %>%
  summarise(n = n()) %>%
  ggplot(aes(n, OCCUPATION_TYPE, fill = OCCUPATION_TYPE)) + geom_col() + ggtitle("OCCUPATION_TYPE", subtitle = "Occupation")

#CNT_FAM_MEMBERS
DataComplete %>%
  group_by(CNT_FAM_MEMBERS) %>%
  ggplot(aes(CNT_FAM_MEMBERS)) + geom_histogram(binwidth = 0.5) + xlim(0,8) + ggtitle("CNT_FAM_MEMBERS", subtitle = "Family size")

#AGE
DataComplete %>%
  group_by(AGE) %>%
  ggplot(aes(AGE)) + geom_histogram(bins = 40) + ggtitle("AGE", subtitle = "Customer age")

#WORKYEARS
DataComplete %>%
  group_by(WORKYEARS) %>%
  filter(between(WORKYEARS,-10,50)) %>% 
  ggplot(aes(WORKYEARS)) + geom_histogram(aes(fill = WORKYEARS > 0), breaks = seq(-10, 50, by=2)) + 
  ggtitle("WORKYEARS", subtitle = "Working years") + guides(fill = FALSE)

#EvaluateCustomer
DataComplete %>%
  group_by(EvaluateCustomer) %>%
  ggplot(aes(EvaluateCustomer)) + geom_histogram(binwidth = 0.5) + ggtitle("EvaluateCustomer", subtitle = "Evaluate customer")

#------------------------------- Summarise -----------------------------------------
#AMT_INCOME_TOTAL
DataComplete %>%
  group_by(AMT_INCOME_TOTAL) %>%
  ggplot(aes(AMT_INCOME_TOTAL)) + geom_boxplot() + xlim(0,500000)

DataComplete %>%
  summarise(Q1 = quantile(AMT_INCOME_TOTAL, 0.25),
            mean = mean(AMT_INCOME_TOTAL),
            median = median(AMT_INCOME_TOTAL),
            Q3 = quantile(AMT_INCOME_TOTAL, 0.75),
            IQR = IQR(AMT_INCOME_TOTAL))

#CNT_FAM_MEMBERS
DataComplete %>%
  group_by(CNT_FAM_MEMBERS) %>%
  ggplot(aes(CNT_FAM_MEMBERS)) + geom_boxplot() + xlim(0,5)

DataComplete %>%
  summarise(Q1 = quantile(CNT_FAM_MEMBERS, 0.25),
            mean = mean(CNT_FAM_MEMBERS),
            median = median(CNT_FAM_MEMBERS),
            Q3 = quantile(CNT_FAM_MEMBERS, 0.75),
            IQR = IQR(CNT_FAM_MEMBERS))

#WORKYEARS
DataComplete %>%
  group_by(WORKYEARS) %>%
  ggplot(aes(WORKYEARS)) + geom_boxplot()

DataComplete %>%
  summarise(Q1 = quantile(WORKYEARS, 0.25),
            mean = mean(WORKYEARS),
            median = median(WORKYEARS),
            Q3 = quantile(WORKYEARS, 0.75),
            IQR = IQR(WORKYEARS))

#AGE
DataComplete %>%
  group_by(AGE) %>%
  ggplot(aes(AGE)) + geom_boxplot()

DataComplete %>%
  summarise(Q1 = quantile(AGE, 0.25),
            mean = mean(AGE),
            median = median(AGE),
            Q3 = quantile(AGE, 0.75),
            IQR = IQR(AGE))