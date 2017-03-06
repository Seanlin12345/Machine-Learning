### HOMEWORK 2 - BIKES ###
## SETUP
set.seed(68)   # Jagr is #68
dev.off()
rm(list=ls())
packageNames = c("MASS", "ISLR", "animation", 
                 "ElemStatLearn", "glmnet", "textir", "nnet", 
                 "methods", "statmod", "stats", "graphics",
                 "RCurl", "jsonlite", "tools", "utils",
                 "data.table", "gbm", "ggplot2", "randomForest",
                 "tree", "class", "kknn", "e1071",
                 "data.table", "recommenderlab",
                 "DAAG", "rpart.plot")

for (pkgName in packageNames){
  if (!(pkgName %in% rownames(installed.packages()))){
    install.packages(pkgName,dependencies=TRUE)}}
source("https://raw.githubusercontent.com/ChicagoBoothML/HelpR/master/docv.R")
library(glmnet)
library(kknn)
library(rpart.plot)
library(tree)
library(rpart)
library(gbm)
library(randomForest)
# TBD: add more if needed

test=read.csv("https://raw.githubusercontent.com/ChicagoBoothML/ML2016/master/hw02/Bike_test.csv")
train=read.csv("https://raw.githubusercontent.com/ChicagoBoothML/ML2016/master/hw02/Bike_train.csv")

#### 1. Variable manipulation  ####

train$holiday <- as.factor(train$holiday) # save as factors
train$season <- as.factor(train$season)

train$weather <- floor(train$weather) ## YT ADDED - round 1.5, 2.5 down
train$weather <- as.factor(train$weather) #Could consider keeping it continuous and keeping the partial values

train$workingday <- as.factor(train$workingday)
train$hour <- as.factor(train$hour)

train$weekday <- train$daylabel %% 7
train$weekday <- ifelse(train$weekday==0,7,train$weekday)
train$weekday <- as.factor(train$weekday) # Note: 1 is Saturday, 2 is Sunday, 3 is Monday, and so on

train$month=as.factor(train$month)

# Remove outlier for atemp, replace with temp
train$atemp <- ifelse((train$atemp < 20 & train$month==8),train$temp,train$atemp)


train$logcount=log(train$count+1)   # Useful particularly for linear regressions


#### 2. Create same variable transformations on test data

test$holiday <- as.factor(test$holiday) # save as factors
test$season <- as.factor(test$season)

test$weather <- floor(test$weather) ## YT ADDED - round 1.5, 2.5 down
test$weather <- as.factor(test$weather) #Could consider keeping it continuous and keeping the partial values

test$workingday <- as.factor(test$workingday)
test$hour <- as.factor(test$hour)

test$weekday <- test$daylabel %% 7
test$weekday <- ifelse(test$weekday==0,7,test$weekday)
test$weekday <- as.factor(test$weekday) # Note: 1 is Saturday, 2 is Sunday, 3 is Monday, and so on

test$month=as.factor(test$month)

# Remove outlier for atemp, replace with temp
test$atemp <- ifelse((test$atemp < 20 & test$month==8),test$temp,test$atemp)


### Keep 
attach(train)
train.data = data.frame(logcount,daylabel,month,hour,holiday,workingday,atemp,humidity,windspeed,weather,temp,weekday)


#### 3. Generate model and predictions ####

boostfit = gbm(logcount~.,data=train.data,
               distribution='gaussian',
               interaction.depth=8,
               n.trees=1000,
               shrinkage=0.1, cv.folds = 5) ###add in cross validation
vsum=summary(boostfit, plotit=F)
print(vsum)

#generate predictions
boost.predictions <- predict(boostfit, test, n.trees = 1000, type = "response")
boost.predictions <- exp(boost.predictions) - 1

## Look at insample error to make sure I'm not doing anything crazy
boost.insample <- predict(boostfit, train, n.trees = 1000, type = "response")
train$pred <- exp(boost.insample) - 1
train$error <- train$count - train$pred
error <- sqrt(mean(train$error^2))

write.csv(boost.predictions,"bike_predictions.csv", row.names = FALSE)




