# Final Project
# REDO STEPS 1 - 3 TO PREPARE DATA
# STEP 7 BEGINS AT LINE 90 
# STEP 7: KNN & SVM - SECONDARY TEST A - MODIFY THE METHOD OF SPLITING INTO BINS
# STEP 1: PRELIMINARY DATA EXPLORATION 

# Read the stores csv data file, which contains store size information
stores<-read.csv("C:/Users/Kula/Desktop/WALMART/stores.csv",header=T,sep=",",na.strings=c("NA", ""))
# Examine stores header 
head(stores)
# stores header includes Store Number, Type, Size

# Read the features csv data file 
features<-read.csv("C:/Users/Kula/Desktop/WALMART/features.csv/features.csv",header=T,sep=",",na.strings=c("NA", ""))
# Examine features header 
head(features)
# Features header includes store number, date, temperature, fuel_price, markdown1-markdown5, CPI, unemployment, IsHoliday

# Read the train csv data file, which contains weekly sales information
train<-read.csv("C:/Users/Kula/Desktop/WALMART/train.csv/train.csv",header=T,sep=",",na.strings=c("NA", ""))
# Examine train header 
head(train)
# train header includes Store Number, Department, Weekly Sales, Whether it was a holiday

# Check for missing values
sum(is.na(stores)==TRUE)
# There are 0 missing values in stores
sum(is.na(features)==TRUE)
# There are 24040 missing values in features
sum(is.na(train)==TRUE)
# There are no missing values in train 


# STEP 2: FEATURE SELECTION  
# &
# STEP 3: CREATING DATAFRAMES FROM CSV FILES

# Store first 70 points
train_70<-head(train,70)
# Check for missing data
is.na(train_70)
# No missing values
# Store Weekly Sales
WklySales<-train_70$Weekly_Sales
# Wish to classify on the basis of weekly sales. Want to divide weekly sales into bins. Considering data from Feb 2/2010 to Oct 26/2012 (the first 145 points).
# Determine min and max of overall data first, so as to determine appropriate bin divisions.
complete_145<-head(train,145)
min(complete_145$Weekly_Sales)
max(complete_145$Weekly_Sales)
# min 14537.27, max 57592.12.
# Will spread range from 10000 to 60000 with increments of 10000.

# Create overall data which we will be working with. Consists of first 145 observations. Assign to variable walmart.
walmart<-head(features,145)

# Feature selection. Will omit Store, Date, Markdown 1-5, and Is Holiday from data.
walmart$Store <- NULL
walmart$Date <- NULL
walmart$MarkDown1 <- NULL
walmart$MarkDown2 <- NULL
walmart$MarkDown3 <- NULL
walmart$MarkDown4 <- NULL
walmart$MarkDown5 <- NULL
walmart$IsHoliday <- NULL

# Descriptive Statistics of overall stores and train
# Average and Standard Deviation of store size
mean(stores$Size)
sd(stores$Size)

# Average and Standard Deviation of weekly sales
mean(train$Weekly_Sales)
sd(train$Weekly_Sales)

# Descriptive Statistics considering selected dataframe walmart
# Average and Standard Deviation of fuel price
mean(walmart$Fuel_Price)
sd(walmart$Fuel_Price)

# Average and Standard Deviation of CPI
mean(walmart$CPI)
sd(walmart$CPI)

# Average and Standard Deviation of Unemployment rate
mean(walmart$Unemployment)
sd(walmart$Unemployment)


# STEP 7: KNN & SVM - SECONDARY TEST A - MODIFY THE METHOD OF SPLITING INTO BINS
# Reduce the levels of rating for quality to bins. Augment to train_70 and complete_145
# Range of values spread from 10000 to 60000. Previously used increments of 10000
# Will now try more bins of division, by having increments of 5000.

Saleslevels<-complete_145$Weekly_Sales
Saleslevels[findInterval(Saleslevels,c(10000,14999))==1L]<-1
Saleslevels[findInterval(Saleslevels,c(15000,19999))==1L]<-2
Saleslevels[findInterval(Saleslevels,c(20000,24999))==1L]<-3
Saleslevels[findInterval(Saleslevels,c(25000,29999))==1L]<-4
Saleslevels[findInterval(Saleslevels,c(30000,34999))==1L]<-5
Saleslevels[findInterval(Saleslevels,c(35000,39999))==1L]<-6
Saleslevels[findInterval(Saleslevels,c(40000,44999))==1L]<-7
Saleslevels[findInterval(Saleslevels,c(45000,49999))==1L]<-8
Saleslevels[findInterval(Saleslevels,c(50000,54999))==1L]<-9
Saleslevels[findInterval(Saleslevels,c(55000,60000))==1L]<-10

train_70$Saleslevels<-Saleslevels[1:70]
complete_145$Saleslevels<-Saleslevels
walmart$SalesLevels<-Saleslevels

walmart$SL<-walmart$SalesLevels
walmart$SL<-factor(walmart$SL)

levels(walmart$SL)<-list(L1=c(1),L2=c(2),L3=c(3),L4=c(4),L5=c(5),L6=c(6),L7=c(7),L8=c(8),L9=c(9),L10=c(10))
walmart$WklyLevels<-complete_145$Weekly_Sales

# Examine correlation
cor(walmart[1:5])

# STEP 5: KNN - USING FEATURES TEMPERATURE, FUEL PRICE, CPI, AND UNEMPLOYMENT TO DETERMINE WEEKLY SALES
# Normalize the data set.
normalize <- function(x) {return ((x - min(x)) / (max(x) - min(x))) }
walmart_n <- as.data.frame(lapply(walmart[1:4], normalize))

# Divide the data to training and testing groups.
walmart_train <- walmart_n[1:70,1:4]
walmart_test <- walmart_n[71:145,1:4]

walmart_train_labels <- walmart[1:70, 6]
walmart_test_labels <- walmart[71:145, 6] 

# Use the KNN algorithm to predict the sales levels using its attributes.
#install.packages("class")
#install.packages("gmodels")
library(class)
library(gmodels)

walmart_test_pred <- knn(train = walmart_train, test = walmart_test,cl = walmart_train_labels, k=3)
# Evaluate the model performance.
table(walmart_test_pred, walmart_test_labels )
CrossTable(x=walmart_test_pred, y=walmart_test_labels,prop.chisq=FALSE)


# STEP 6: SVM - USING FEATURES TEMPERATURE, FUEL PRICE, CPI, AND UNEMPLOYMENT TO DETERMINE WEEKLY SALES
#install.packages('e1071')

library('e1071')
library(rpart)
head(walmart,5)
# test on same points as tested on KNN so as to compare to KNN
walmart_select<-walmart[71:145,]
x <- subset(walmart_select, select=-SL)
y <- walmart$SL
svm_model <- svm(SL ~ ., data=walmart_select)
summary(svm_model)
# test with train data
pred_select <- predict(svm_model, x)

# Check accuracy:
table(pred_select, walmart_test_labels )
CrossTable(x=pred_select, y=walmart_test_labels,prop.chisq=FALSE)

# perform for 1-145 points
x <- subset(walmart, select=-SL)
y <- walmart$SL
svm_model <- svm(SL ~ ., data=walmart)
summary(svm_model)
# test with train data
pred <- predict(svm_model, x)

# Check accuracy:
table(pred, y)
CrossTable(x=pred, y=y,prop.chisq=FALSE)


