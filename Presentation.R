library(psych)
library(glmnet)
library(dplyr)
library(caret)
library(nnet)

setwd("~/Desktop/McGill/Winter 2023/PSYC 560/Presentation")
mydata.obese = read.csv('obesity.csv')

# Making data
mydata_training=mydata.obese[1:1000,]
mydata_test=mydata.obese[1001:2111,]

# Prepping training data
mydata_training$Gender = as.factor(mydata_training$Gender)
mydata_training$Age = as.numeric(mydata_training$Age)
mydata_training$Height = as.numeric(mydata_training$Height)
mydata_training$Weight = as.numeric(mydata_training$Weight)
mydata_training$family_history_with_overweight = as.factor(mydata_training$family_history_with_overweight)
mydata_training$FAVC = as.factor(mydata_training$FAVC)
mydata_training$FCVC = as.factor(mydata_training$FCVC)
mydata_training$NCP = as.factor(mydata_training$NCP)
mydata_training$CAEC = as.factor(mydata_training$CAEC)
mydata_training$SMOKE = as.factor(mydata_training$SMOKE)
mydata_training$CH2O = as.factor(mydata_training$CH2O)
mydata_training$SCC = as.factor(mydata_training$SCC)
mydata_training$FAF = as.factor(mydata_training$FAF)
mydata_training$TUE = as.factor(mydata_training$TUE)
mydata_training$CALC = as.factor(mydata_training$CALC)
mydata_training$MTRANS = as.factor(mydata_training$MTRANS)
mydata_training <- mydata_training %>%
  mutate(Insufficient_Weight = ifelse(NObeyesdad == "Insufficient_Weight", 1, 0),
         Normal_Weight = ifelse(NObeyesdad == "Normal_Weight", 1, 0),
         Overweight_Level_I = ifelse(NObeyesdad == "Overweight_Level_I", 1, 0),
         Overweight_Level_II = ifelse(NObeyesdad == "Overweight_Level_II", 1, 0),
         Obesity_Type_I = ifelse(NObeyesdad == "Obesity_Type_I", 1, 0),
         Obesity_Type_II = ifelse(NObeyesdad == "Obesity_Type_II", 1, 0),
         Obesity_Type_III = ifelse(NObeyesdad == "Obesity_Type_III", 1, 0)) %>%
  select(-NObeyesdad)

# Prepping test data
mydata_test$Gender = as.factor(mydata_test$Gender)
mydata_test$Age = as.numeric(mydata_test$Age)
mydata_test$Height = as.numeric(mydata_test$Height)
mydata_test$Weight = as.numeric(mydata_test$Weight)
mydata_test$family_history_with_overweight = as.factor(mydata_test$family_history_with_overweight)
mydata_test$FAVC = as.factor(mydata_test$FAVC)
mydata_test$FCVC = as.factor(mydata_test$FCVC)
mydata_test$NCP = as.factor(mydata_test$NCP)
mydata_test$CAEC = as.factor(mydata_test$CAEC)
mydata_test$SMOKE = as.factor(mydata_test$SMOKE)
mydata_test$CH2O = as.factor(mydata_test$CH2O)
mydata_test$SCC = as.factor(mydata_test$SCC)
mydata_test$FAF = as.factor(mydata_test$FAF)
mydata_test$TUE = as.factor(mydata_test$TUE)
mydata_test$CALC = as.factor(mydata_test$CALC)
mydata_test$MTRANS = as.factor(mydata_test$MTRANS)
mydata_test <- mydata_test %>%
  mutate(Insufficient_Weight = ifelse(NObeyesdad == "Insufficient_Weight", 1, 0),
         Normal_Weight = ifelse(NObeyesdad == "Normal_Weight", 1, 0),
         Overweight_Level_I = ifelse(NObeyesdad == "Overweight_Level_I", 1, 0),
         Overweight_Level_II = ifelse(NObeyesdad == "Overweight_Level_II", 1, 0),
         Obesity_Type_I = ifelse(NObeyesdad == "Obesity_Type_I", 1, 0),
         Obesity_Type_II = ifelse(NObeyesdad == "Obesity_Type_II", 1, 0),
         Obesity_Type_III = ifelse(NObeyesdad == "Obesity_Type_III", 1, 0)) %>%
  select(-NObeyesdad)


# Linear regression
lm.fit = lm(Normal_Weight ~ Weight + FAVC + SCC, data = mydata_training)
summary(lm.fit)
lm.pred.tt = predict(lm.fit, mydata_test)
lm.pred.tt.MSE = mean((lm.pred.tt - mydata_test$Normal_Weight)^2)
cat("\n MSE(LR): ", lm.pred.tt.MSE)
