library(psych)
library(glmnet)
library(dplyr)
library(caret)
library(nnet)
library(tree) 
library(randomForest)
library(gbm)
library(pROC)

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
mydata.tr$FCVC = as.numeric(mydata.tr$FCVC)
mydata.tr$NCP = as.numeric(mydata.tr$NCP)
mydata.tr$CAEC = as.factor(mydata.tr$CAEC)
mydata.tr$SMOKE = as.factor(mydata.tr$SMOKE)
mydata.tr$CH2O = as.numeric(mydata.tr$CH2O)
mydata.tr$SCC = as.factor(mydata.tr$SCC)
mydata.tr$FAF = as.numeric(mydata.tr$FAF)
mydata.tr$TUE = as.numeric(mydata.tr$TUE)
mydata.tr$CALC = as.factor(mydata.tr$CALC)
mydata.tr$MTRANS = as.factor(mydata.tr$MTRANS)
mydata.tr$NObeyesdad = factor(mydata.tr$NObeyesdad, levels = c('Insufficient_Weight', 'Normal_Weight', 'Overweight_Level_I',
                                                               'Overweight_Level_II', 'Obesity_Type_I', 'Obesity_Type_II',
                                                               'Obesity_Type_III'),
                              labels = c(1, 2, 3, 4, 5, 6, 7))
mydata.tr$NObeyesdad = as.numeric(mydata.tr$NObeyesdad)

# Prepping test data
mydata.tt$Gender = as.factor(mydata.tt$Gender)
mydata.tt$Age = as.numeric(mydata.tt$Age)
mydata.tt$Height = as.numeric(mydata.tt$Height)
mydata.tt$Weight = as.numeric(mydata.tt$Weight)
mydata.tt$family_history_with_overweight = as.factor(mydata.tt$family_history_with_overweight)
mydata.tt$FAVC = as.factor(mydata.tt$FAVC)
mydata.tt$FCVC = as.numeric(mydata.tt$FCVC)
mydata.tt$NCP = as.numeric(mydata.tt$NCP)
mydata.tt$CAEC = as.factor(mydata.tt$CAEC)
mydata.tt$SMOKE = as.factor(mydata.tt$SMOKE)
mydata.tt$CH2O = as.numeric(mydata.tt$CH2O)
mydata.tt$SCC = as.factor(mydata.tt$SCC)
mydata.tt$FAF = as.numeric(mydata.tt$FAF)
mydata.tt$TUE = as.numeric(mydata.tt$TUE)
mydata.tt$CALC = as.factor(mydata.tt$CALC)
mydata.tt$MTRANS = as.factor(mydata.tt$MTRANS)
mydata.tt$NObeyesdad = factor(mydata.tt$NObeyesdad, levels = c('Insufficient_Weight', 'Normal_Weight', 'Overweight_Level_I',
                                                               'Overweight_Level_II', 'Obesity_Type_I', 'Obesity_Type_II',
                                                               'Obesity_Type_III'),
                              labels = c(1, 2, 3, 4, 5, 6, 7))
mydata.tt$NObeyesdad = as.numeric(mydata.tt$NObeyesdad)

# 1. Linear regression

# 1.1 Simple linear regression MSE = 0.3789012
lm.fit = lm(NObeyesdad ~ ., mydata.tr)
summary(lm.fit)

# 1.2 Optimized linear regression MSE = 0.3724206
lm.fit = lm(NObeyesdad ~ Age + Height + Weight + family_history_with_overweight +
              FCVC + NCP + CAEC + SMOKE + CH2O + FAF + TUE + CALC + MTRANS, mydata.tr)

# 1.3 MSE in predicting test data
lm.pred.tt = predict(lm.fit, mydata.tt)
lm.pred.tt.MSE = mean((lm.pred.tt - mydata.tt$NObeyesdad)^2)
cat("\n MSE(LR): ", lm.pred.tt.MSE)

# 2. KNN regression 

# knnmodel = knnreg(NObeyesdad ~ Age + Height + Weight + family_history_with_overweight + FAVC
#                     FCVC + NCP + CAEC + SMOKE + CH2O + SCC + FAF + TUE + CALC + MTRANS, mydata.tr, k = 5)

# 2.1 MSE = 0.6336084
knnmodel = knnreg(NObeyesdad ~ Height + Weight + family_history_with_overweight + FAF +
                    FCVC + SMOKE + TUE, mydata.tr, k = 5)

pred_y_knn = predict(knnmodel, mydata.tt)

# 2.2 MSE in predicting test data
MSE_knn = mean((mydata.tt$NObeyesdad - pred_y_knn)^2)
cat("\n MSE(KNN): ", MSE_knn)

# 3. Regression tree

# 3.1 MSE = 0.4411145
# rtree.fit = tree(NObeyesdad ~ ., mydata.tr)
rtree.fit = tree(NObeyesdad ~ Gender + Height + Weight, mydata.tr)
# rtree.fit
# plot(rtree.fit)
# text(rtree.fit)
# summary(rtree.fit)

# Pruning the tree ---
rtree.cv = cv.tree(rtree.fit, K = 5)
plot(rtree.cv$size,rtree.cv$dev, type ="o", xlab = "# of terminal nodes", ylab = "CVE")  
# rtree.cv
list_CVE=cbind(rtree.cv$size,rtree.cv$dev)
colnames(list_CVE)=c('# of terminal nodes','CVE')
list_CVE

rtree.fit.pruned = prune.tree(rtree.fit, best = 9)
# rtree.fit.pruned
# plot(rtree.fit.pruned)
# text(rtree.fit.pruned)

# Test the tree ---
rtree.pred.y.tt = predict(rtree.fit, mydata.tt)
rtree.pred.y.tt.pruned = predict(rtree.fit.pruned, mydata.tt)
rtree.original.MSE = mean((rtree.pred.y.tt - mydata.tt$NObeyesdad)^2)
rtree.pruned.MSE = mean((rtree.pred.y.tt.pruned - mydata.tt$NObeyesdad)^2)
cat("\n MSE_original(RT): ", rtree.original.MSE,
    "\n MSE_pruned(RT)  : ", rtree.pruned.MSE)

# mydata.tr <- mydata.tr %>%
#   mutate(Insufficient_Weight = ifelse(NObeyesdad == "Insufficient_Weight", 1, 0),
#          Normal_Weight = ifelse(NObeyesdad == "Normal_Weight", 1, 0),
#          Overweight_Level_I = ifelse(NObeyesdad == "Overweight_Level_I", 1, 0),
#          Overweight_Level_II = ifelse(NObeyesdad == "Overweight_Level_II", 1, 0),
#          Obesity_Type_I = ifelse(NObeyesdad == "Obesity_Type_I", 1, 0),
#          Obesity_Type_II = ifelse(NObeyesdad == "Obesity_Type_II", 1, 0),
#          Obesity_Type_III = ifelse(NObeyesdad == "Obesity_Type_III", 1, 0)) %>%
#   select(-NObeyesdad)

# mydata.tt <- mydata.tt %>%
#   mutate(Insufficient_Weight = ifelse(NObeyesdad == "Insufficient_Weight", 1, 0),
#          Normal_Weight = ifelse(NObeyesdad == "Normal_Weight", 1, 0),
#          Overweight_Level_I = ifelse(NObeyesdad == "Overweight_Level_I", 1, 0),
#          Overweight_Level_II = ifelse(NObeyesdad == "Overweight_Level_II", 1, 0),
#          Obesity_Type_I = ifelse(NObeyesdad == "Obesity_Type_I", 1, 0),
#          Obesity_Type_II = ifelse(NObeyesdad == "Obesity_Type_II", 1, 0),
#          Obesity_Type_III = ifelse(NObeyesdad == "Obesity_Type_III", 1, 0)) %>%
#   select(-NObeyesdad)