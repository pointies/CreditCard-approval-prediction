library(readr)
library(dplyr)
library(tidyr)
library(caret)
library(rpart)
library(rpart.plot)

#Import dataset
DataComplete <- read_csv("DataComplete.csv")
DataComplete

#------------------------------- Logistic regression -----------------------------------------
DataComplete$EvaluateCustomer <- as.factor(DataComplete$EvaluateCustomer)
DataComplete$CODE_GENDER <- as.factor(DataComplete$CODE_GENDER)
DataComplete$FLAG_OWN_CAR <- as.factor(DataComplete$FLAG_OWN_CAR)
DataComplete$FLAG_OWN_REALTY <- as.factor(DataComplete$FLAG_OWN_REALTY)
DataComplete$NAME_INCOME_TYPE <- as.factor(DataComplete$NAME_INCOME_TYPE)
DataComplete$NAME_EDUCATION_TYPE <- as.factor(DataComplete$NAME_EDUCATION_TYPE)
DataComplete$NAME_FAMILY_STATUS <- as.factor(DataComplete$NAME_FAMILY_STATUS)
DataComplete$NAME_HOUSING_TYPE <- as.factor(DataComplete$NAME_HOUSING_TYPE)
DataComplete$FLAG_WORK_PHONE <- ifelse(test = DataComplete$FLAG_WORK_PHONE == 1, yes = "Y", no = "N")
DataComplete$FLAG_WORK_PHONE <- as.factor(DataComplete$FLAG_WORK_PHONE)
DataComplete$FLAG_PHONE <- ifelse(test = DataComplete$FLAG_PHONE == 1, yes = "Y", no = "N")
DataComplete$FLAG_PHONE <- as.factor(DataComplete$FLAG_PHONE)
DataComplete$FLAG_EMAIL <- ifelse(test = DataComplete$FLAG_EMAIL == 1, yes = "Y", no = "N")
DataComplete$FLAG_EMAIL <- as.factor(DataComplete$FLAG_EMAIL)
DataComplete$OCCUPATION_TYPE <- as.factor(DataComplete$OCCUPATION_TYPE)
str(DataComplete)

#Split data
set.seed(1)
sample <- sample(c(TRUE, FALSE), nrow(DataComplete), replace=TRUE, prob=c(0.7,0.3))
train  <- DataComplete[sample, ]
test   <- DataComplete[!sample, ]

#Modeling
model <- glm(EvaluateCustomer ~ CODE_GENDER + FLAG_OWN_CAR + FLAG_OWN_REALTY + CNT_CHILDREN
             + AMT_INCOME_TOTAL + NAME_INCOME_TYPE + NAME_EDUCATION_TYPE + NAME_FAMILY_STATUS
             + NAME_HOUSING_TYPE + FLAG_WORK_PHONE + FLAG_PHONE + FLAG_EMAIL + CNT_FAM_MEMBERS 
             + AGE + WORKYEARS, train, family = binomial)
summary(model)

#------------------------------- Predict -----------------------------------------
res1 <- predict(model, test, type = "response")
hist(res1)
res1c <- factor(ifelse(res1 > 0.75, "1", "0"))
table(res1c)
confusionMatrix(res1c, test$EvaluateCustomer, mode = "prec_recall", positive = "1")

#------------------------------- Decision Tree -----------------------------------------
#Tree No cp
tree <- rpart(EvaluateCustomer ~ CODE_GENDER + FLAG_OWN_CAR + FLAG_OWN_REALTY + CNT_CHILDREN
              + AMT_INCOME_TOTAL + NAME_INCOME_TYPE + NAME_EDUCATION_TYPE + NAME_FAMILY_STATUS
              + NAME_HOUSING_TYPE + FLAG_WORK_PHONE + FLAG_PHONE + FLAG_EMAIL + CNT_FAM_MEMBERS 
              + AGE + WORKYEARS, train)
rpart.plot(tree, box.palette="RdBu", shadow.col="gray", nn=TRUE)

#Tree Cross-validation
train_control <- trainControl(method="cv",number = 5)
model <- train(EvaluateCustomer ~ CODE_GENDER + FLAG_OWN_CAR + FLAG_OWN_REALTY + CNT_CHILDREN
               + AMT_INCOME_TOTAL + NAME_INCOME_TYPE + NAME_EDUCATION_TYPE + NAME_FAMILY_STATUS
               + NAME_HOUSING_TYPE + FLAG_WORK_PHONE + FLAG_PHONE + FLAG_EMAIL + CNT_FAM_MEMBERS 
               + AGE + WORKYEARS, train,
               trControl = train_control,
               method = "rpart")
model

#Tree cp Before
tree <- rpart(EvaluateCustomer ~ CODE_GENDER + FLAG_OWN_CAR + FLAG_OWN_REALTY + CNT_CHILDREN
              + AMT_INCOME_TOTAL + NAME_INCOME_TYPE + NAME_EDUCATION_TYPE + NAME_FAMILY_STATUS
              + NAME_HOUSING_TYPE + FLAG_WORK_PHONE + FLAG_PHONE + FLAG_EMAIL + CNT_FAM_MEMBERS 
              + AGE + WORKYEARS, train, parms=list(split=c("information","gini")),
              cp = 0.0004, minsplit=20, minbucket=5, maxdepth=30)
tree
tree$variable.importance
res1 <- predict(tree, test, type = "class")
confusionMatrix(res1, test$EvaluateCustomer, mode = "prec_recall", positive = "1")

#Tree cp After
tree <- rpart(EvaluateCustomer ~ CODE_GENDER + FLAG_OWN_CAR + FLAG_OWN_REALTY + CNT_CHILDREN
              + AMT_INCOME_TOTAL + NAME_INCOME_TYPE + NAME_EDUCATION_TYPE + NAME_FAMILY_STATUS
              + NAME_HOUSING_TYPE + FLAG_WORK_PHONE + FLAG_PHONE + FLAG_EMAIL + CNT_FAM_MEMBERS 
              + AGE + WORKYEARS, train, parms=list(split=c("information","gini")),
              cp = 0.0003,minsplit=7, minbucket=5, maxdepth=10)
tree
rpart.plot(tree, box.palette="RdBu", shadow.col="gray", nn=TRUE)
tree$variable.importance
res1 <- predict(tree, test, type = "class")
confusionMatrix(res1, test$EvaluateCustomer, mode = "prec_recall", positive = "1")
