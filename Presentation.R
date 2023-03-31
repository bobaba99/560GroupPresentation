library(psych)
library(glmnet)
library(dplyr)
library(caret)
library(nnet)
library(tree) 
library(randomForest)
library(gbm)

setwd("~/Desktop/McGill/Winter 2023/PSYC 560/Presentation")
mydata.obese = read.csv('obesity.csv')

# Making data
mydata.tr=mydata.obese[1:1000,]
mydata.tt=mydata.obese[1001:2111,]

# Prepping training data
mydata.tr$Gender = as.factor(mydata.tr$Gender)
mydata.tr$Age = as.numeric(mydata.tr$Age)
mydata.tr$Height = as.numeric(mydata.tr$Height)
mydata.tr$Weight = as.numeric(mydata.tr$Weight)
mydata.tr$family_history_with_overweight = as.factor(mydata.tr$family_history_with_overweight)
mydata.tr$FAVC = as.factor(mydata.tr$FAVC)
mydata.tr$FCVC = as.factor(mydata.tr$FCVC)
mydata.tr$NCP = as.factor(mydata.tr$NCP)
mydata.tr$CAEC = as.factor(mydata.tr$CAEC)
mydata.tr$SMOKE = as.factor(mydata.tr$SMOKE)
mydata.tr$CH2O = as.factor(mydata.tr$CH2O)
mydata.tr$SCC = as.factor(mydata.tr$SCC)
mydata.tr$FAF = as.factor(mydata.tr$FAF)
mydata.tr$TUE = as.factor(mydata.tr$TUE)
mydata.tr$CALC = as.factor(mydata.tr$CALC)
mydata.tr$MTRANS = as.factor(mydata.tr$MTRANS)
mydata.tr <- mydata.tr %>%
  mutate(Insufficient_Weight = ifelse(NObeyesdad == "Insufficient_Weight", 1, 0),
         Normal_Weight = ifelse(NObeyesdad == "Normal_Weight", 1, 0),
         Overweight_Level_I = ifelse(NObeyesdad == "Overweight_Level_I", 1, 0),
         Overweight_Level_II = ifelse(NObeyesdad == "Overweight_Level_II", 1, 0),
         Obesity_Type_I = ifelse(NObeyesdad == "Obesity_Type_I", 1, 0),
         Obesity_Type_II = ifelse(NObeyesdad == "Obesity_Type_II", 1, 0),
         Obesity_Type_III = ifelse(NObeyesdad == "Obesity_Type_III", 1, 0)) %>%
  select(-NObeyesdad)

# Prepping test data
mydata.tt$Gender = as.factor(mydata.tt$Gender)
mydata.tt$Age = as.numeric(mydata.tt$Age)
mydata.tt$Height = as.numeric(mydata.tt$Height)
mydata.tt$Weight = as.numeric(mydata.tt$Weight)
mydata.tt$family_history_with_overweight = as.factor(mydata.tt$family_history_with_overweight)
mydata.tt$FAVC = as.factor(mydata.tt$FAVC)
mydata.tt$FCVC = as.factor(mydata.tt$FCVC)
mydata.tt$NCP = as.factor(mydata.tt$NCP)
mydata.tt$CAEC = as.factor(mydata.tt$CAEC)
mydata.tt$SMOKE = as.factor(mydata.tt$SMOKE)
mydata.tt$CH2O = as.factor(mydata.tt$CH2O)
mydata.tt$SCC = as.factor(mydata.tt$SCC)
mydata.tt$FAF = as.factor(mydata.tt$FAF)
mydata.tt$TUE = as.factor(mydata.tt$TUE)
mydata.tt$CALC = as.factor(mydata.tt$CALC)
mydata.tt$MTRANS = as.factor(mydata.tt$MTRANS)
mydata.tt <- mydata.tt %>%
  mutate(Insufficient_Weight = ifelse(NObeyesdad == "Insufficient_Weight", 1, 0),
         Normal_Weight = ifelse(NObeyesdad == "Normal_Weight", 1, 0),
         Overweight_Level_I = ifelse(NObeyesdad == "Overweight_Level_I", 1, 0),
         Overweight_Level_II = ifelse(NObeyesdad == "Overweight_Level_II", 1, 0),
         Obesity_Type_I = ifelse(NObeyesdad == "Obesity_Type_I", 1, 0),
         Obesity_Type_II = ifelse(NObeyesdad == "Obesity_Type_II", 1, 0),
         Obesity_Type_III = ifelse(NObeyesdad == "Obesity_Type_III", 1, 0)) %>%
  select(-NObeyesdad)


# Linear regression
lm.fit = lm(Normal_Weight ~ Gender + Age + Height + Weight + family_history_with_overweight + 
              FAVC + FCVC + NCP + CAEC + SMOKE + CH2O + SCC +
              FAF + TUE + CALC + MTRANS, mydata.tr)
summary(lm.fit)
lm.pred.tt = predict(lm.fit, mydata.tt)
lm.pred.tt.MSE = mean((lm.pred.tt - mydata.tt$Normal_Weight)^2)
cat("\n MSE(LR): ", lm.pred.tt.MSE)

# KNN regression
# knnmodel = knnreg(Normal_Weight ~ ., mydata.tr, k = 5)
# pred_y_knn = predict(knnmodel, mydata.tt)
# MSE_knn = mean((mydata.tt$Normal_Weight - pred_y_knn)^2)

# Regression tree
rtree.fit = tree(Normal_Weight ~ family_history_with_overweight + Age + Height, mydata.tr)  
rtree.fit
plot(rtree.fit)
text(rtree.fit)
summary(rtree.fit)

# Pruning the tree ---
rtree.cv = cv.tree(rtree.fit, K = 5)
plot(rtree.cv$size,rtree.cv$dev, type ="o", xlab = "# of terminal nodes", ylab = "CVE")  
rtree.cv
list_CVE=cbind(rtree.cv$size,rtree.cv$dev)
colnames(list_CVE)=c('# of terminal nodes','CVE')
list_CVE

rtree.fit.pruned = prune.tree(rtree.fit, best = 6)
rtree.fit.pruned
plot(rtree.fit.pruned)
text(rtree.fit.pruned)

# Test the tree ---
rtree.pred.y.tt = predict(rtree.fit, mydata.tt)
rtree.pred.y.tt.pruned = predict(rtree.fit.pruned, mydata.tt)
rtree.original.MSE = mean((rtree.pred.y.tt - mydata.tt$Normal_Weight)^2)
rtree.pruned.MSE = mean((rtree.pred.y.tt.pruned - mydata.tt$Normal_Weight)^2)
cat("\n MSE_original(RT): ", rtree.original.MSE,
    "\n MSE_pruned(RT)  : ", rtree.pruned.MSE)
