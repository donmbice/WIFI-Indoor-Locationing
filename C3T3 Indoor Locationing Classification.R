

# # DataSource: Joaquín Torres-Sospedra, Raúl Montoliu, Adolfo Martínez-Usó, Tomar J. Arnau, Joan P. Avariento, Mauri Benedito-Bordonau, 
# Joaquín Huerta UJIIndoorLoc: A New Multi-building and Multi-floor Database for WLAN Fingerprint-based Indoor Localization Problems In 
# Proceedings of the Fifth International Conference on Indoor Positioning and Indoor Navigation, 2014. 
# https://archive.ics.uci.edu/ml/datasets/ujiindoorloc

###############
# Project Notes
###############
# Investigate feasibility of using "wifi fingerprinting to determine a person's location in indoor spaces.
# We have been provided with a large database of wifi fingerprints for a multi-building industrial campus with a 
# location (building, floor, and location ID) associated with each fingerprint. Task is to evaluate multiple machine
# learning models to see which produces the best result, enabling us to make a recommendation to the client. 
# If your recommended model is sufficiently accurate, it will be incorporated into a smartphone app for 
# indoor locationing.
# 

################
# Load packages
################
library(tidyr)
library(dplyr)
library(ggplot2)
library(caret)
library(C50)
library(e1071)
library(doMC)

#####################
# Parallel processing
#####################

#--- for OSX ---#
detectCores()   
registerDoMC(cores = 4)


###Load the Data
getwd()
setwd("/Users/donbice/Desktop/R data files and pipeline/UJIndoorLoc")

trainingData <- read.csv("trainingData.csv", stringsAsFactors = FALSE, header = T)

################
# Evaluate data
################

#--- Dataset 1 ---#

# check for missing values 
anyNA(trainingData) #FALSE
is.na(trainingData) 

#Summary of non-WAP attributes
summary(trainingData[,521:529]) #19937 obs. of 529 vars.

> summary(trainingData[,521:529]) #19937 obs. of 529 vars.
# LONGITUDE        LATITUDE           FLOOR         BUILDINGID       SPACEID      RELATIVEPOSITION     USERID          PHONEID        TIMESTAMP        
# Min.   :-7691   Min.   :4864746   Min.   :0.000   Min.   :0.000   Min.   :  1.0   Min.   :1.000    Min.   : 1.000   Min.   : 1.00   Min.   :1.370e+09  
# 1st Qu.:-7595   1st Qu.:4864821   1st Qu.:1.000   1st Qu.:0.000   1st Qu.:110.0   1st Qu.:2.000    1st Qu.: 5.000   1st Qu.: 8.00   1st Qu.:1.371e+09  
# Median :-7423   Median :4864852   Median :2.000   Median :1.000   Median :129.0   Median :2.000    Median :11.000   Median :13.00   Median :1.372e+09  
# Mean   :-7464   Mean   :4864871   Mean   :1.675   Mean   :1.213   Mean   :148.4   Mean   :1.833    Mean   : 9.068   Mean   :13.02   Mean   :1.371e+09  
# 3rd Qu.:-7359   3rd Qu.:4864930   3rd Qu.:3.000   3rd Qu.:2.000   3rd Qu.:207.0   3rd Qu.:2.000    3rd Qu.:13.000   3rd Qu.:14.00   3rd Qu.:1.372e+09  
# Max.   :-7301   Max.   :4865017   Max.   :4.000   Max.   :2.000   Max.   :254.0   Max.   :2.000    Max.   :18.000   Max.   :24.00   Max.   :1.372e+09  


str(trainingData[,521:529])
# 'data.frame':	19937 obs. of  9 variables:
# $ LONGITUDE       : num  -7541 -7537 -7519 -7525 -7632 ...
# $ LATITUDE        : num  4864921 4864934 4864950 4864934 4864982 ...
# $ FLOOR           : int  2 2 2 2 0 2 2 2 2 2 ...
# $ BUILDINGID      : int  1 1 1 1 0 1 1 1 1 1 ...
# $ SPACEID         : int  106 106 103 102 122 105 103 101 112 103 ...
# $ RELATIVEPOSITION: int  2 2 2 2 2 2 2 2 2 1 ...
# $ USERID          : int  2 2 2 2 11 2 2 2 2 2 ...
# $ PHONEID         : int  23 23 23 23 13 23 23 23 23 23 ...
# $ TIMESTAMP       : int  1371713733 1371713691 1371714095 1371713807 1369909710 1371713841 1371713883 1371713775 1371714307 1371714128 ...


#Plot locations with data sets
plot(trainingData$LONGITUDE,trainingData$LATITUDE)


###############
# Preprocessing
###############


#Combine BUILDINGID, FLOOR, SPACEID into composite attribute
train.UID <- unite(trainingData, "BLDG_FL_SPACE", remove = TRUE, sep = ".", c("BUILDINGID","FLOOR", "SPACEID"))

summary(train.UID[,520:526]) #19937 obs. of 530 vars.

str(train.UID[,520:526])

#Remove independent variables that are not WAP 1-520 leaving BLDG_FL_SPACE_POS
train.UID$TIMESTAMP <- NULL # remove ID
train.UID$PHONEID <- NULL
train.UID$USERID <- NULL
train.UID$LONGITUDE <- NULL
train.UID$LATITUDE <- NULL
train.UID$RELATIVEPOSITION <- NULL

#Convert UID to factor datatype
train.UID$BLDG_FL_SPACE <- as.factor(train.UID$BLDG_FL_SPACE)
str(train.UID[,518:521]) 


################
# Sampling
################
# # create 20% sample for train data set
set.seed(123) # set random seed
Train20p <- train.UID[sample(1:nrow(train.UID), round(nrow(train.UID)*.2),replace=FALSE),]
nrow(Train20p) #3987 obs of 530 var
head(Train20p) # ensure randomness


Train20p <- Train20p %>% droplevels()
str(Train20p$BLDG_FL_SPACE)

##################
# Train/test sets
##################
# set random seed
set.seed(123) 
# create the training partition that is 75% of total obs
inTraining <- createDataPartition(Train20p$BLDG_FL_SPACE, p=0.75, list=FALSE)
# create training/testing dataset
trainSet <- Train20p[inTraining,]   
testSet <- Train20p[-inTraining,]   
# verify number of obs 
nrow(trainSet) #
nrow(testSet) #


################
# Train control
################
# set 10 fold cross validation
fitControl <- trainControl(method = "repeatedcv", number = 10, repeats = 10)


##############
# Train model
##############

##----- C5.0 -------##

# C5.0 train/fit
set.seed(123)
c50Fit <- train(BLDG_FL_SPACE ~., data = trainSet, method = "C5.0", trControl = fitControl, tuneLength = 2)

c50Fit
# evaluate predictor variables
predictors(c50Fit)
# eval variable importance
varImp(c50Fit)


## ------- RF ------- ##

# RF train/fit
set.seed(123)
rfFit <- train(BLDG_FL_SPACE ~., data=trainSet, method="rf", importance=T, trControl=fitControl) 
rfFit
# eval pred vars
predictors(rfFit)
# eval variable imp
varImp(rfFit)
# performance metrics

##----- k NN -------##
#k NN train/fit
set.seed(123)
kNNFit <- train(BLDG_FL_SPACE ~ ., data = trainSet, method = "knn", trControl = fitControl)

kNNFit
# eval pred vars
predictors(kNNFit)
# eval variable imp
varImp(kNNFit)
# performance metrics

## ------- SVM ------- ##

# SVM train/fit
set.seed(123)
SVMFit <- train(BLDG_FL_SPACE ~ ., data = trainSet, method = "SVMLinear", trControl = fitControl)
svmFit2 <- train(BLDG_FL_SPACE~., data=trainSet, method="svmLinear2", trControl=fitControl) ##this one
svmFit2
# eval pred vars
predictors(SVMFit)
# eval variable imp
varImp(SVMFit)
# performance metrics


##--- Compare metrics ---##

ModelFitResults <- resamples(list(c50 = c50Fit, kNN = kNNFit, rf = rfFit, SVM = SVMFit))
# output summary metrics for tuned models 
summary(ModelFitResults)

####Chart alpha and kappa performance metrics for each algorithm tested###




