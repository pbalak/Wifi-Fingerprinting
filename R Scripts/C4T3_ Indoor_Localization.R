## C4T3 - Eveluate techniques - wifi locationling
## Last Update : 2018.01.19
## File : C4T3_Deep_Data_Analytics_Initial_Data_Analysis.R

## Project Name: Course 4 Task3

##################
## Project Notes
##################
#Project Summary :Just to explore the data and gain some insights into it

######################
## HouseKeeping ##
#####################
#clear objects

rm(list=ls())

#get working directory
getwd()


################
# Load Packages
################
install.packages("caret")
install.packages("corrplot")
install.packages("readr")
#install.packages("doMC", repos="http://R-Forge.R-project.org")    # parallel processing

### parallel processing in Windows #####
library(doParallel) 
cl <- makeCluster(detectCores(), type='PSOCK')
registerDoParallel(cl)
cl<-1
stopCluster(cl)
########### Check number of cores and workers available 
detectCores()
getDoParWorkers()
############

library(caret) # Classification and Regression trianing
library(janitor) # For cleaning the input attributes.Need to use clean_names() while importing the data.
##library(binr)
library(arules) # For Discretize
library(readr) 
library(doMC)
library(corrplot)
require(dplyr)
require(tidyr)
require(lubridate)
require(ggplot2)

###############################################
## Import Data
###############################################

## train set
wifi_train_set=read.csv("C:/Users/Sheetal/Desktop/Big Data and Data Analytics/Module 4/Task3/DataSet/UJIndoorLoc/UJIndoorLoc/trainingData.csv",header = TRUE,stringsAsFactors = FALSE)

dim(wifi_train_set)
nrow(wifi_train_set)
class(wifi_train_set)
head(wifi_train_set)
str(wifi_train_set)
summary(wifi_train_set)
any(is.na(wifi_train_set))

summary(wifi_train_set$BUILDINGID)
str(wifi_train_set$BUILDINGID)
str(wifi_train_set$FLOOR)
summary(wifi_train_set$FLOOR)
summary(wifi_train_set$SPACEID)
summary(wifi_train_set$RELATIVEPOSITION)
str(wifi_train_set$RELATIVEPOSITION)
summary(wifi_train_set$USERID)
str(wifi_train_set$USERID)
summary(wifi_train_set$PHONEID)


## Histograms

hist(wifi_train_set$FLOOR)
hist(wifi_train_set$BUILDINGID)
hist(wifi_train_set$SPACEID)
hist(wifi_train_set$BUILDINGID)
hist(wifi_train_set$USERID)
hist(wifi_train_set$RELATIVEPOSITION)
hist(wifi_train_set$PHONEID)

## Frequency
table(wifi_train_set$FLOOR)
table(wifi_train_set$BUILDINGID)
table(wifi_train_set$SPACEID)
table(wifi_train_set$BUILDINGID)
table(wifi_train_set$USERID)
table(wifi_train_set$RELATIVEPOSITION)
table(wifi_train_set$PHONEID)

## Building count by floor

table(wifi_train_set$BUILDINGID,wifi_train_set$FLOOR)
table(wifi_train_set$USERID,wifi_train_set$BUILDINGID)
table(wifi_train_set$USERID,wifi_train_set$PHONEID)


## Taking a back up of the training set
wifi_train_set_All_Attributes <- wifi_train_set


#############################
#Feature Removal
#############################

# Remove ID and other obvious features
wifi_train_set$TIMESTAMP <- NULL

names(wifi_train_set)


## Remove based on correlation

#Building ID is associated closely with Longitude and Latitude - Multi- co linearity

wifi_train_set_526<-wifi_train_set
wifi_train_set_526$LONGITUDE<-NULL
wifi_train_set_526$LATITUDE<-NULL

names(wifi_train_set_526[521:526])


############################
## Conversion of data types
###########################

#wifi_train_set$FLOOR<-as.factor(wifi_train_set$FLOOR)
#summary(wifi_train_set$FLOOR)
#nlevels(wifi_train_set$FLOOR)
#
#
#wifi_train_set$BUILDINGID<-as.factor(wifi_train_set$BUILDINGID)
#summary(wifi_train_set$BUILDINGID)
#nlevels(wifi_train_set$BUILDINGID)
#
#wifi_train_set$USERID<-as.factor(wifi_train_set$USERID) ## Label 1
#summary(wifi_train_set$USERID)
#nlevels(wifi_train_set$USERID)
#
#wifi_train_set$SPACEID<-as.factor(wifi_train_set$SPACEID) ## Label 2
#summary(wifi_train_set$SPACEID)
#nlevels(wifi_train_set$SPACEID)
#
#wifi_train_set$RELATIVEPOSITION<-as.factor(wifi_train_set$RELATIVEPOSITION) ## Label 3
#summary(wifi_train_set$RELATIVEPOSITION)
#nlevels(wifi_train_set$RELATIVEPOSITION)
#
#wifi_train_set$PHONEID<-as.factor(wifi_train_set$PHONEID) 
#summary(wifi_train_set$PHONEID)
#nlevels(wifi_train_set$PHONEID)
#
#
### Adding a new column -> 
#
#### Factors for wifi_train_set_526
#
#wifi_train_set_526$FLOOR<-as.factor(wifi_train_set$FLOOR)
#wifi_train_set_526$BUILDINGID<-as.factor(wifi_train_set$BUILDINGID)
#wifi_train_set_526$USERID<-as.factor(wifi_train_set$USERID) ## Label 1
#wifi_train_set_526$SPACEID<-as.factor(wifi_train_set$SPACEID) ## Label 2
#wifi_train_set_526$RELATIVEPOSITION<-as.factor(wifi_train_set$RELATIVEPOSITION) ## Label 3
#wifi_train_set_526$PHONEID<-as.factor(wifi_train_set$PHONEID) 
#
#names(wifi_train_set_526[521:526])
#
#wifi_train_set_526<-unite(wifi_train_set_526,SPACE_RELPOS,c(SPACEID,RELATIVEPOSITION),remove = FALSE)
#names(wifi_train_set_526[521:527])
#head(wifi_train_set_526)
#nrow(wifi_train_set_526)
#str(wifi_train_set_526$SPACE_RELPOS)
#wifi_train_set_526$SPACE_RELPOS<-as.factor(wifi_train_set_526$SPACE_RELPOS)
#nlevels(wifi_train_set_526$SPACE_RELPOS)
#
##Removing Space and Relative Position
#
#wifi_train_set_526$SPACEID<-wifi_train_set_526$RELATIVEPOSITION<-NULL
#wifi_train_set_525<-wifi_train_set_526 # since the no of attributes is now 525
#names(wifi_train_set_525[521:525])
#
###Re-Ordering the columns in the data frame
#
#wifi_train_set_525<-wifi_train_set_525[c(1:520,521,522,525,523,524)] # User and SPACE_RELPOS are two labels that needs to be predicted
#
#names(wifi_train_set_525[521:525])
#str(wifi_train_set_525[521:525])
#
#wifi_train_set_525$FLOOR<-as.factor(wifi_train_set_525$FLOOR)
#wifi_train_set_525$BUILDINGID<-as.factor(wifi_train_set_525$BUILDINGID)
#wifi_train_set_525$PHONEID<-as.factor(wifi_train_set_525$PHONEID)
#wifi_train_set_525$SPACE_RELPOS<-as.factor(wifi_train_set_525$SPACE_RELPOS)
#wifi_train_set_525$USERID<-as.factor(wifi_train_set_525$USERID)

############################
## Sampling
############################

# Create dataset with sample size lesser of 1k or dataset size for RFE and train
#set.seed(7)
#
#wifi_train_set_1K<- wifi_train_set[sample(1:nrow(wifi_train_set),1000,replace = FALSE),]
#nrow(wifi_train_set_1K)
#
#
### Another Sample after converting the datatypes into factors
#
#set.seed(7)
#
#wifi_train_set_1K_factors<- wifi_train_set[sample(1:nrow(wifi_train_set),1000,replace = FALSE),]
#nrow(wifi_train_set_1K_factors)
#summary(wifi_train_set_1K_factors[521:528])
#
### Task 1 - Sampling for 525 attributes 
#
#set.seed(99)
#
#wifi_train_set_525_sample<- wifi_train_set_525[sample(1:nrow(wifi_train_set_525),10000,replace = FALSE),]
#nrow(wifi_train_set_525_sample)
#summary(wifi_train_set_525_sample[521:525])

################################
## Filter Engineering Methods
################################

# Correlation

names(wifi_train_set[521:528])


corr <- cor(wifi_train_set[,521:528])
corr

# Plot

corrplot(corr, order = "hclust")
corrplot(corr, method = "circle")

#######################################
## Train the sample##
######################################

## SVN , K-NN , rf

#########################
##Train Control
########################


##### DO NOT USE######
#fitControl <- trainControl(method = "repeatedcv",number = 10, repeats = 1)
#
#set.seed(99)
#
##### Train the Sample set#####
#knnFitSample1 <- train(SPACE_RELPOS ~ WAP001:PHONEID , data = wifi_train_set_525_sample, method = "knn", trControl = fitControl, metric="Accuracy",tuneLength=1, preProc = "range")
#knnFitSample1
#
#knnFitSample2 <- train(USERID ~ WAP001:PHONEID , data = wifi_train_set_525_sample, method = "knn", trControl = fitControl, metric="Accuracy",tuneLength=20, preProc = "range")
#knnFitSample2 
#
#
#rFitSample1 <- train(SPACE_RELPOS ~ WAP001:PHONEID , data = wifi_train_set_525_sample, method = "rf", trControl = fitControl, importance =T , preProc = "range")
#rFitSample1
#
#names(wifi_train_set_525_sample[521:525])
#summary(wifi_train_set_525_sample$SPACE_RELPOS)
#str(wifi_train_set_525_sample$SPACE_RELPOS)
#levels(wifi_train_set_525_sample$SPACE_RELPOS)
#########################################################################

# Since random sample was throwing error (few factors did not get the values-  Space Id and Rel position - might be unique across buildings and cross validation were missing some of them ),
#restricting the sample to Specific building - Building 2 was the highest observation


#head(wifi_train_set)
#names(wifi_train_set[521:528])
#
#wifi_train_set_Building2<-wifi_train_set
#
#wifi_train_set_Building2$LONGITUDE<-wifi_train_set_Building2$LATITUDE<-NULL
#
## Filtering Building 2
#
#wifi_train_set_Building2<-wifi_train_set_Building2%>%filter(BUILDINGID == 2)
#nrow(wifi_train_set_Building2)
#
## combining Space Id with Rel position
#
#wifi_train_set_Building2<-unite(wifi_train_set_Building2,SPACE_RELPOS,c(SPACEID,RELATIVEPOSITION),remove = FALSE)
#names(wifi_train_set_Building2[521:527])
#
#wifi_train_set_Building2_525v<-wifi_train_set_Building2
#wifi_train_set_Building2_525v$SPACEID<-wifi_train_set_Building2_525v$RELATIVEPOSITION<-NULL
#str(wifi_train_set_Building2_525v[521:525])
#
## Moving Phone Id upfront
#
#wifi_train_set_Building2_525v<-wifi_train_set_Building2_525v[c(1:520,521,522,525,523,524)] # User and SPACE_RELPOS are two labels that needs to be predicted
#
#wifi_train_set_Building2_525v$FLOOR<-as.factor(wifi_train_set_Building2_525v$FLOOR)
#wifi_train_set_Building2_525v$BUILDINGID<-as.factor(wifi_train_set_Building2_525v$BUILDINGID)
#wifi_train_set_Building2_525v$PHONEID<-as.factor(wifi_train_set_Building2_525v$PHONEID)
#wifi_train_set_Building2_525v$SPACE_RELPOS<-as.factor(wifi_train_set_Building2_525v$SPACE_RELPOS)
#wifi_train_set_Building2_525v$USERID<-as.factor(wifi_train_set_Building2_525v$USERID)

#######################################
## Train the sample##
######################################

## SVN , K-NN , rf

#########################
##Train Control
########################

fitControl <- trainControl(method = "repeatedcv",number = 10, repeats = 10)

set.seed(1234)


##### Train the Sample set#####
#knnFitSample1 <- train(SPACE_RELPOS ~ WAP001:PHONEID , data = wifi_train_set_Building2_525v, method = "knn", trControl = fitControl, metric="Accuracy",tuneLength=1, preProc = "range")
#knnFitSample1
#
#knnFitSample2 <- train(USERID ~ WAP001:PHONEID , data = wifi_train_set_Building2_525v, method = "knn", trControl = fitControl, metric="Accuracy",tuneLength=20, preProc = "range")
#knnFitSample2 
#
#
#rFitSample1 <- train(SPACE_RELPOS ~ WAP001:PHONEID , data = wifi_train_set_525_sample, method = "rf", trControl = fitControl, importance =T , preProc = "range")
#rFitSample1
#
#######################################################
#### Filtering again - Building 2 Floor 2
#######################################################
#
#wifi_train_set_Building2_Floor2<-wifi_train_set
#
#wifi_train_set_Building2_Floor2$LONGITUDE<-wifi_train_set_Building2$LATITUDE<-NULL
#
## Filtering Building 2
#
#wifi_train_set_Building2_Floor2<-wifi_train_set_Building2_Floor2%>%filter(BUILDINGID == 2)%>%filter(FLOOR == 2)
#nrow(wifi_train_set_Building2_Floor2)
#
## combining Space Id with Rel position
#
#wifi_train_set_Building2_Floor2<-unite(wifi_train_set_Building2_Floor2,SPACE_RELPOS,c(SPACEID,RELATIVEPOSITION),remove = FALSE)
#names(wifi_train_set_Building2_Floor2[521:527])
#
#wifi_train_set_Building2_Floor2_525v<-wifi_train_set_Building2_Floor2
#wifi_train_set_Building2_Floor2_525v$SPACEID<-wifi_train_set_Building2_Floor2_525v$RELATIVEPOSITION<-NULL
#str(wifi_train_set_Building2_Floor2_525v[521:526])
#
## Moving Phone Id upfront
#
#wifi_train_set_Building2_Floor2_525v<-wifi_train_set_Building2_Floor2_525v[c(1:520,521,522,525,523,524)] # User and SPACE_RELPOS are two labels that needs to be predicted
#
### Removing Floor and Building as we are filtering it to be Building =2 and Floor =2
#wifi_train_set_Building2_Floor2_523v<-wifi_train_set_Building2_Floor2_525v
#wifi_train_set_Building2_Floor2_523v$FLOOR<-wifi_train_set_Building2_Floor2_523v$BUILDINGID<-NULL
#
#wifi_train_set_Building2_Floor2_523v$PHONEID<-as.factor(wifi_train_set_Building2_Floor2_523v$PHONEID)
#wifi_train_set_Building2_Floor2_523v$SPACE_RELPOS<-as.factor(wifi_train_set_Building2_Floor2_523v$SPACE_RELPOS)
#wifi_train_set_Building2_Floor2_523v$USERID<-as.factor(wifi_train_set_Building2_Floor2_523v$USERID)
#
#names(wifi_train_set_Building2_Floor2_523v[521:523])
#

######################################################################################################
### Filtering again - Building 2 Floor 2 and combining Building ID, space, Floor and Rel Pos into one
######################################################################################################

wifi_train_set_Building2_Floor2<-wifi_train_set_526

# Filtering Building 2

wifi_train_set_Building2_Floor2<-wifi_train_set_Building2_Floor2%>%filter(BUILDINGID == 2)%>%filter(FLOOR == 2)

dim(wifi_train_set_Building2_Floor2)

# combining Space Id , Building ID, Floor and Rel. Position

wifi_train_set_Building2_Floor2<-unite(wifi_train_set_Building2_Floor2,MASTERLOC,c(BUILDINGID,FLOOR,SPACEID,RELATIVEPOSITION),remove = FALSE)
names(wifi_train_set_Building2_Floor2[521:527])
head(wifi_train_set_Building2_Floor2$MASTERLOC)

wifi_train_set_Building2_Floor2_523v<-wifi_train_set_Building2_Floor2
wifi_train_set_Building2_Floor2_523v$SPACEID<-wifi_train_set_Building2_Floor2_523v$RELATIVEPOSITION<-NULL
wifi_train_set_Building2_Floor2_523v$BUILDINGID<-wifi_train_set_Building2_Floor2_523v$FLOOR<-NULL

str(wifi_train_set_Building2_Floor2_523v[521:523])


wifi_train_set_Building2_Floor2_523v$PHONEID<-as.factor(wifi_train_set_Building2_Floor2_523v$PHONEID)
wifi_train_set_Building2_Floor2_523v$MASTERLOC<-as.factor(wifi_train_set_Building2_Floor2_523v$MASTERLOC)
wifi_train_set_Building2_Floor2_523v$USERID<-as.factor(wifi_train_set_Building2_Floor2_523v$USERID)

names(wifi_train_set_Building2_Floor2_523v[521:523])
str(wifi_train_set_Building2_Floor2_523v[521:523])
## Splitting 80% and 20% - Train and Test Set ####

#############################
##Creating Train/Test Sets
#############################

## createDataPartition to split the data for 75% training set and 25% test set

set.seed(1234)
train_index <- createDataPartition(y=wifi_train_set_Building2_Floor2_523v$MASTERLOC,p=.80,list = FALSE)

train_Set <- wifi_train_set_Building2_Floor2_523v[train_index,]
test_Set <- wifi_train_set_Building2_Floor2_523v[-train_index,]

nrow(train_Set)
nrow(test_Set)
str(train_Set)
head(train_Set)
tail(train_Set)

### Train the training set 

knnFit1 <- train(MASTERLOC ~ . , data = train_Set, method = "knn", trControl = fitControl, metric="Accuracy",tuneLength=20, preProc = "range")
knnFit1
predictors(knnFit1)

#k   Accuracy   Kappa    
#5  0.6028630  0.5960381

## Saving the model

saveRDS(knnFit1,"knnFit1.rds")

## Prediction
knnPredict1<- predict(knnFit1,newdata = test_Set)
knnPredict1
summary(knnPredict1)

postResample(knnPredict1,test_Set$MASTERLOC)

# Accuracy     Kappa 
#0.6666667 0.6610016


View(cbind(knnPredict1,test_Set$MASTERLOC))
View(cbind(as.character(knnPredict1),as.character(test_Set$MASTERLOC)))

knn_Out<-cbind(as.character(knnPredict1),as.character(test_Set$MASTERLOC))

colnames(knn_Out)<- c("Predicted Value","Actual Value")

knn_Out
## Random Forest 

rFit1 <- train(MASTERLOC ~ . , data = train_Set, method = "rf", trControl = fitControl, importance=T, preProc = "range")
rFit1

#mtry  Accuracy    Kappa     
#2   0.07940605  0.05080182
#32   0.86640241  0.86399842
#524   0.83588606  0.83296428

## Saving the model

saveRDS(rFit1,"rFit1.rds")

## Prediction
rPredict1<- predict(rFit1,newdata = test_Set)
rPredict1
summary(rPredict1)

postResample(rPredict1,test_Set$MASTERLOC)

#Accuracy     Kappa 
#0.9000000 0.8982855 


View(cbind(rPredict1,test_Set$MASTERLOC))
View(cbind(as.character(rPredict1),as.character(test_Set$MASTERLOC)))


rOut<-cbind(as.character(rPredict1),as.character(test_Set$MASTERLOC))
colnames(rOut)<-c("Predicted Value","Actual Value")
rOut


##SVM

set.seed(1234)
SVMCaretRadial <- train(MASTERLOC ~ .,data = train_Set, importance = T, method="svmRadial" , preProcess = c("center", "scale"),
                        tuneLength = 10)
# if SCALE is not used, its erroring out and found that if there are constant values, it does so. 
SVMCaretLinear <-train(MASTERLOC ~ .,data = train_Set, importance = T, method="svmLinear" ,  preProcess = c("center", "scale"),
                       trControl = fitControl)

SVMCaretLinear

# Accuracy   Kappa    
#0.6732503  0.6675838

SVMCaretLinearPredict1<- predict(SVMCaretLinear,newdata = test_Set)
SVMCaretLinearPredict1


postResample(SVMCaretLinearPredict1,test_Set$MASTERLOC)
confusionMatrix(SVMCaretLinearPredict1,test_Set$MASTERLOC)

SVMOut<-cbind(as.character(SVMCaretLinearPredict1),as.character(test_Set$MASTERLOC))
colnames(SVMOut)<-c("Predicted Value","Actual Value")
SVMOut

#Accuracy     Kappa 
#0.7000000 0.6950359 



#Using gbm

GBMCaret <- train(MASTERLOC ~ ., data = train_Set, distribution="gaussian", method="gbm",
                  trControl=fitControl, verbose=FALSE, 
                  bag.fraction=0.75 , metric = "Accuracy")
GBMCaret
varImp(GBMCaret)
plot(varImp(GBMCaret,scale = TRUE))

## Bagged CART

baggedCART<-train(MASTERLOC ~ . , data = train_Set, method = "treebag", trControl = fitControl)
baggedCART
#Accuracy   Kappa   
#0.7906982  0.7869962

## Prediction
bagPredict1<- predict(baggedCART,newdata = test_Set)
bagPredict1
summary(bagPredict1)

postResample(bagPredict1,test_Set$MASTERLOC)

#Accuracy     Kappa 
#0.8233333 0.8203512


CARTOut<-cbind(as.character(bagPredict1),as.character(test_Set$MASTERLOC))
colnames(CARTOut)<-c("Predicted Value","Actual Value")
CARTOut

## Resamples

Resample <- resamples(list(BaggedCART=baggedCART,KNN=knnFit1,RandomForest=rFit1,SVM=SVMCaretLinear))

summary(Resample)
dotplot(Resample)


## Algorithm Tune - Random Forest

# Grid search

trainControl<- trainControl(method="repeatedcv", number=10, repeats=3, search="grid")
set.seed(1234)

## Random forest was best at mtry =32 and hence searching for 3-31.
tunegrid <- expand.grid(.mtry=c(3:31)) 

rGrid <- train(MASTERLOC ~ . , data = train_Set, method = "rf", metric="Accuracy", tuneGrid=tunegrid, trControl=trainControl)
print(rGrid)
plot(rGrid)
## task 

## Summary of each model and its results

summary(rFit1)
