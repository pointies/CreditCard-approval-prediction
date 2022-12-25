library(readr)
library(dplyr)
library(tidyr)
library(tidyverse)
library(ggplot2)

#Import dataset
credit_record <- read_csv("credit_record.csv")
application_record <- read_csv("application_record.csv")

#Join dataset
creditJoin <- inner_join(credit_record, application_record, by = "ID")
creditJoin <- drop_na(creditJoin,`CODE_GENDER`)
creditJoin

#Change STATUS
creditJoin$STATUSYN <- replace(creditJoin$STATUSYN, creditJoin$STATUS == "C",0)
creditJoin$STATUSYN <- replace(creditJoin$STATUSYN, creditJoin$STATUS == "X",0)
creditJoin$STATUSYN <- replace(creditJoin$STATUSYN, creditJoin$STATUS == "0",1)
creditJoin$STATUSYN <- replace(creditJoin$STATUSYN, creditJoin$STATUS == "1",2)
creditJoin$STATUSYN <- replace(creditJoin$STATUSYN, creditJoin$STATUS == "2",3)
creditJoin$STATUSYN <- replace(creditJoin$STATUSYN, creditJoin$STATUS == "3",4)
creditJoin$STATUSYN <- replace(creditJoin$STATUSYN, creditJoin$STATUS == "4",5)
creditJoin$STATUSYN <- replace(creditJoin$STATUSYN, creditJoin$STATUS == "5",6)

#Score customer
creditJoin %>%
  group_by(ID) %>%
  summarise(Score = mean(STATUSYN)) -> Scorecustomer

#EvaluteCustomer
Scorecustomer <- mutate(Scorecustomer, 
                        EvaluateCustomer = ifelse(Score <= 0.9, 1, 0)) 
hist(Scorecustomer$EvaluateCustomer)

#JoinData predict
DataComplete <- inner_join(Scorecustomer, application_record, by = "ID")
DataComplete$OCCUPATION_TYPE <- replace_na('Unspecified')
DataComplete <- mutate(DataComplete, AGE = DAYS_BIRTH/-365)
DataComplete$AGE <- as.integer(DataComplete$AGE)
DataComplete <- mutate(DataComplete, EMPLOYED = DAYS_EMPLOYED/-365)
DataComplete$WORKYEARS <- as.integer(DataComplete$EMPLOYED)
DataComplete$WORKYEARS <- replace(DataComplete$WORKYEARS, DataComplete$WORKYEARS == -1000,-1)

DataComplete %>%
  select(-DAYS_BIRTH,-DAYS_EMPLOYED,-EMPLOYED,-FLAG_MOBIL) -> DataComplete
DataComplete

#Export data
write.csv(DataComplete, "DataComplete.csv")