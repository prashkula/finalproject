# Final Project
# REDO STEPS 1 - 3 TO PREPARE DATA WITH ADDITIONAL MODIFICATIONS TO ALLOW FOR SIZE MODIFICATION

# STEP 8: KNN & SVM - SECONDARY TEST B - MODIFY DATA SET SIZE


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



# Feature selection. Will omit Store, Date, Markdown 1-5, and Is Holiday from data.
features$Store <- NULL
features$Date <- NULL
features$MarkDown1 <- NULL
features$MarkDown2 <- NULL
features$MarkDown3 <- NULL
features$MarkDown4 <- NULL
features$MarkDown5 <- NULL
features$IsHoliday <- NULL

# Check for missing values
sum(is.na(stores)==TRUE)
# There are 0 missing values in stores
sum(is.na(features)==TRUE)
# There are 24040 missing values in features
# Which column contains NA
sum(is.na(features$Temperature)==TRUE)
sum(is.na(features$Fuel_Price)==TRUE)
sum(is.na(features$CPI)==TRUE)
sum(is.na(features$Unemployment)==TRUE)
# CPI and unemployment contain NA
sum(is.na(train)==TRUE)
# There are no missing values in train 


# Drop rows with missing values
train<-train[!(is.na(features$CPI)==TRUE),]
stores<-stores[!(is.na(features$CPI)==TRUE),]
features<-features[!(is.na(features$CPI)==TRUE),]
# Check if any NA remaining
sum(is.na(features$CPI)==TRUE)
sum(is.na(features$Unemployment)==TRUE)


# STEP 2: FEATURE SELECTION  
# &
# STEP 3: CREATING DATAFRAMES FROM CSV FILES


# Store first 20000 points
train_2000<-head(train,2000)
# Check for missing data
sum(is.na(train_2000)==TRUE)
# No missing values
# Store Weekly Sales
WklySales<-train_2000$Weekly_Sales
# Wish to classify on the basis of weekly sales. Want to divide weekly sales into bins. Considering data from Feb 2/2010 to Oct 26/2012 (the first 5000 points).
# Determine min and max of overall data first, so as to determine appropriate bin divisions.
complete_5000<-head(train,5000)
min(complete_5000$Weekly_Sales)
max(complete_5000$Weekly_Sales)
# min  -139.65, max 127811.9
# Examine spread
hist(complete_5000$Weekly_Sale)
median(complete_5000$Weekly_Sale)
# Median 9337.9
hist(train$Weekly_Sales[train$Weekly_Sales<10000])
hist(train$Weekly_Sales[train$Weekly_Sales>10000])
# If have too large increments, wont provide informative results. 
# However, creating too many bins is cumbersome 
sum((complete_5000$Weekly_Sale>10000)==TRUE)
sum((complete_5000$Weekly_Sale<10000)==TRUE)
# Approximately midway division at 10000
# Will create staggered bins from -139.65 to 127811 
# First 10 bins go from -500 to 9999 with increments of 1050
# Second 10 bins go from 10000 to 1300000 with increments of 12000

# Create overall data which we will be working with. Consists of first 5000 observations. Assign to variable walmart.
walmart<-head(features,5000)

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

# STEP 4: REDUCE NUMERICAL WEEKLY SALES INTO LEVELS FOR CLASSIFICATION
# Reduce the levels of rating for quality to bins. Augment to train_20000 and complete_391462
Saleslevels<-complete_5000$Weekly_Sales
Saleslevels[findInterval(Saleslevels,c(-1000,12099))==1L]<-1
Saleslevels[findInterval(Saleslevels,c(12100,25199))==1L]<-2
Saleslevels[findInterval(Saleslevels,c(25200,38299))==1L]<-3
Saleslevels[findInterval(Saleslevels,c(38300,51399))==1L]<-4
Saleslevels[findInterval(Saleslevels,c(51400,64499))==1L]<-5
Saleslevels[findInterval(Saleslevels,c(64500,77599))==1L]<-6
Saleslevels[findInterval(Saleslevels,c(77600,90699))==1L]<-7
Saleslevels[findInterval(Saleslevels,c(90700,103799))==1L]<-8
Saleslevels[findInterval(Saleslevels,c(103800,116899))==1L]<-9
Saleslevels[findInterval(Saleslevels,c(116900,130000))==1L]<-10


train_2000$Saleslevels<-Saleslevels[1:2000]
complete_5000$Saleslevels<-Saleslevels
walmart$SalesLevels<-Saleslevels

walmart$SL<-walmart$SalesLevels
walmart$SL<-factor(walmart$SL)

levels(walmart$SL)<-list(L1=c(1),L2=c(2),L3=c(3),L4=c(4),L5=c(5),L6=c(6),L7=c(7),L8=c(8),L9=c(9),L10=c(10))

walmart$WklyLevels<-complete_5000$Weekly_Sales

walmart<-walmart[!(is.na(walmart$SL)==TRUE),]


# Examine correlation
cor(walmart[1:5])

# STEP 5: KNN - USING FEATURES TEMPERATURE, FUEL PRICE, CPI, AND UNEMPLOYMENT TO DETERMINE WEEKLY SALES
# Normalize the data set.
normalize <- function(x) {return ((x - min(x)) / (max(x) - min(x))) }
walmart_n <- as.data.frame(lapply(walmart[1:4], normalize))


# Divide the data to training and testing groups.
walmart_train <- walmart[1:2000,1:4]
walmart_test <- walmart[2001:4000,1:4]

walmart_train_labels <- walmart[1:2000, 6]
walmart_test_labels <- walmart[2001:4000, 6] 

sum(is.na(walmart_train)==TRUE)
sum(is.na(walmart_train_labels)==TRUE)

sum(is.na(walmart_test)==TRUE)
sum(is.na(walmart_test_labels)==TRUE)

# Drop rows with missing values
train<-train[!(is.na(features$CPI)==TRUE),]

# Use the KNN algorithm to predict the sales levels using its attributes.
install.packages("class")
install.packages("gmodels")
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
walmart_select<-walmart[2001:4000,]
x <- subset(walmart_select, select=-SL)
y <- walmart$SL
svm_model <- svm(SL ~ ., data=walmart_select)
summary(svm_model)
# test with train data
pred_select <- predict(svm_model, x)


# Check accuracy:
table(pred_select, walmart_test_labels )
CrossTable(x=pred_select, y=walmart_test_labels,prop.chisq=FALSE)

# perform for 1-4000 points
x <- subset(walmart, select=-SL)
y <- walmart$SL
svm_model <- svm(SL ~ ., data=walmart)
summary(svm_model)
# test with train data
pred <- predict(svm_model, x)

# Check accuracy:
table(pred, y)
CrossTable(x=pred, y=y,prop.chisq=FALSE)


