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
library(doMC)

#####################
# Parallel processing
#####################

#--- for OSX ---#
detectCores()   
registerDoMC(cores = 4)

# method 2
# Check number of cores and workers available 
detectCores()
getDoParWorkers()
cl <- makeCluster(detectCores()-1, type='PSOCK')


###Load the Data
getwd()
setwd("/Users/donbice/Desktop/R data files and pipeline/UJIndoorLoc")


train <- read.csv("trainingData.csv", stringsAsFactors = FALSE, header = T)
valid <- read.csv("validationData.csv", stringsAsFactors = FALSE, header = T)


################
# Evaluate data
################

#--- Dataset 1 ---#

# check for missing values 
anyNA(train) #FALSE
is.na(train) 

#Summary of non-WAP attributes
summary(train[,521:529]) #19937 obs. of 529 vars.

# LONGITUDE        LATITUDE           FLOOR         BUILDINGID       SPACEID      RELATIVEPOSITION     USERID          PHONEID     
# Min.   :-7691   Min.   :4864746   Min.   :0.000   Min.   :0.000   Min.   :  1.0   Min.   :1.000    Min.   : 1.000   Min.   : 1.00  
# 1st Qu.:-7595   1st Qu.:4864821   1st Qu.:1.000   1st Qu.:0.000   1st Qu.:110.0   1st Qu.:2.000    1st Qu.: 5.000   1st Qu.: 8.00  
# Median :-7423   Median :4864852   Median :2.000   Median :1.000   Median :129.0   Median :2.000    Median :11.000   Median :13.00  
# Mean   :-7464   Mean   :4864871   Mean   :1.675   Mean   :1.213   Mean   :148.4   Mean   :1.833    Mean   : 9.068   Mean   :13.02  
# 3rd Qu.:-7359   3rd Qu.:4864930   3rd Qu.:3.000   3rd Qu.:2.000   3rd Qu.:207.0   3rd Qu.:2.000    3rd Qu.:13.000   3rd Qu.:14.00  
# Max.   :-7301   Max.   :4865017   Max.   :4.000   Max.   :2.000   Max.   :254.0   Max.   :2.000    Max.   :18.000   Max.   :24.00  
# TIMESTAMP        
# Min.   :1.370e+09  
# 1st Qu.:1.371e+09  
# Median :1.372e+09  
# Mean   :1.371e+09  
# 3rd Qu.:1.372e+09  
# Max.   :1.372e+09  


str(train[,521:529])

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
plot(train$LONGITUDE,train$LATITUDE)


#--- Dataset 2 ---#

# check for missing values 
anyNA(valid) #FALSE
is.na(valid) 

summary(valid[,521:529]) #1111 obs. of 529 vars.

# LONGITUDE        LATITUDE           FLOOR         BUILDINGID        SPACEID  RELATIVEPOSITION     USERID     PHONEID     
# Min.   :-7696   Min.   :4864748   Min.   :0.000   Min.   :0.0000   Min.   :0   Min.   :0        Min.   :0   Min.   : 0.00  
# 1st Qu.:-7637   1st Qu.:4864843   1st Qu.:1.000   1st Qu.:0.0000   1st Qu.:0   1st Qu.:0        1st Qu.:0   1st Qu.: 9.00  
# Median :-7560   Median :4864915   Median :1.000   Median :1.0000   Median :0   Median :0        Median :0   Median :13.00  
# Mean   :-7529   Mean   :4864902   Mean   :1.572   Mean   :0.7588   Mean   :0   Mean   :0        Mean   :0   Mean   :11.92  
# 3rd Qu.:-7421   3rd Qu.:4864967   3rd Qu.:2.000   3rd Qu.:1.0000   3rd Qu.:0   3rd Qu.:0        3rd Qu.:0   3rd Qu.:15.00  
# Max.   :-7300   Max.   :4865017   Max.   :4.000   Max.   :2.0000   Max.   :0   Max.   :0        Max.   :0   Max.   :21.00  
# TIMESTAMP        
# Min.   :1.380e+09  
# 1st Qu.:1.380e+09  
# Median :1.381e+09  
# Mean   :1.381e+09  
# 3rd Qu.:1.381e+09  
# Max.   :1.381e+09  

str(valid[,521:529])

#Plot locations with data sets
plot(valid$LONGITUDE, valid$LATITUDE)


###############
# Preprocessing
###############
#Combine BUILDINGID, FLOOR, SPACEID, and RELATIVEPOSITION into composite attribute.  If remove = TRUE, remove input columns from dataframe.
train.UID <- unite(train, "BLDG_FL_SPACE_POS", remove = TRUE, sep = ".", c("BUILDINGID", "FLOOR","SPACEID","RELATIVEPOSITION"))
ncol(train.UID)   
summary(train.UID[,521:530]) #19937 obs. of 530 vars.
str(train.UID[,521:530])

valid.UID <- unite(valid, "BLDG_FL_SPACE_POS", remove = TRUE, sep = ".", c("BUILDINGID", "FLOOR","SPACEID","RELATIVEPOSITION"))
ncol(valid.UID)   
summary(valid.UID[,520:526]) #1111 obs. of 530 vars.
str(valid.UID[,520:526])

#Convert UID to factor datatype
train.UID$BLDG_FL_SPACE_POS <- as.factor(train.UID$BLDG_FL_SPACE_POS)
valid.UID$BLDG_FL_SPACE_POS <- as.factor(valid.UID$BLDG_FL_SPACE_POS)

#Remove independent variables that are not WAP 1-520 leaving BLDG_FL_SPACE_POS
train.UID$TIMESTAMP <- NULL # remove ID
train.UID$PHONEID <- NULL
train.UID$USERID <- NULL
train.UID$LONGITUDE <- NULL
train.UID$LATITUDE <- NULL
str(train.UID[,520:521]) 

valid.UID$TIMESTAMP <- NULL # remove ID
valid.UID$PHONEID <- NULL
valid.UID$USERID <- NULL
valid.UID$LONGITUDE <- NULL
valid.UID$LATITUDE <- NULL
str(valid.UID[,518:521]) 


################
# Sampling
################

# # ---- Sampling ---- #

# # create 20% sample for train data set
set.seed(123) # set random seed
Train20p <- train.UID[sample(1:nrow(train.UID), round(nrow(train.UID)*.2),replace=FALSE),]
nrow(Train20p) #3987 obs of 521 var
head(Train20p) # ensure randomness


##################
# Train/test sets
##################
# set random seed
set.seed(123) 
# create the training partition that is 75% of total obs
inTraining <- createDataPartition(Train20p$BLDG_FL_SPACE_POS, p=0.75, list=FALSE)
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
fitControl <- trainControl(method = "repeatedcv", number = 10, repeats = 10, returnData = FALSE, trim = TRUE)

#fitControl <- trainControl(method = "cv", number = 10)

##############
# Train model
##############

##----- k NN -------##
#k NN train/fit
set.seed(123)
kNNFit <- train(BLDG_FL_SPACE_POS ~ ., data = trainSet, method = "knn", metric = "Accuracy", trControl = fitControl)

kNNFit
# eval pred vars
predictors(kNNFit)
# eval variable imp
varImp(kNNFit)
# performance metrics

##----- C5.0 -------##

# C5.0 train/fit
set.seed(123)
c50Fit <- train(BLDG_FL_SPACE_POS~., data = trainSet, method = "C5.0", trControl = fitControl, tuneLength = 2)

c50Fit
# evaluate predictor variables
predictors(c50Fit)
# eval variable importance
varImp(c50Fit)


## ------- RF ------- ##

# RF train/fit
set.seed(123)
rfFit <- train(BLDG_FL_SPACE_POS~., data=trainSet, method="rf", importance=T, trControl=fitControl) 
rfFit
# eval pred vars
predictors(rfFit)
# eval variable imp
varImp(rfFit)
# performance metrics


## ------- SVM ------- ##

# SVM train/fit
set.seed(123)
SVMFit <- train(BLDG_FL_SPACE_POS ~ ., data = trainSet, method = "SVM", trControl = fitControl)

SVMFit
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




