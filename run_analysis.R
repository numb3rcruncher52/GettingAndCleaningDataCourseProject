## run_analysis.R
## 
## Author: Max
## Date: 4/14/2016
##
## This script downloads accelerometer data from the Samsung Galaxy S
## smartphone and cleans it up for analysis for the Getting and Cleaning
## data course project from Coursera

library(dplyr)

## Load the subject, activity, and feature data for both the test and training
## data sets from the UCI HAR Dataset in the project directory
test_features <- read.table("./UCI HAR Dataset/test/X_test.txt")
test_activities <- read.table("./UCI HAR Dataset/test/y_test.txt")
test_subject <- read.table("./UCI HAR Dataset/test/subject_test.txt")
test <- cbind(test_subject,test_activities,test_features)

train_features <- read.table("./UCI HAR Dataset/train/X_train.txt")
train_activities <- read.table("./UCI HAR Dataset/train/y_train.txt")
train_subject <- read.table("./UCI HAR Dataset/train/subject_train.txt")
train <- cbind(train_subject,train_activities,train_features)

feature_names <- as.character(read.table("./UCI HAR Dataset/features.txt")$V2)
feature_names <- c("subject","activity",feature_names)

## Merge training and test sets to get one dataframe
data <- rbind(train,test)

## Extract only mean and standard deviation for each measurement
final_features <- grep("-mean\\(|-std\\(|^subject$|^activity$",feature_names)
data <- data[,final_features]

## Relabel the variables to be descriptive

feature_names <- feature_names[final_features]
# replace 't' with time and 'f' with fourier
feature_names <- gsub("^t","time",feature_names)
feature_names <- gsub("^f","fourier",feature_names)
# replace 'Acc' with accelerometer and 'Gyro' with gyroscope
feature_names <- gsub("Acc", "Accelerometer", feature_names)
feature_names <- gsub("Gyro", "Gyroscope", feature_names)
# replace 'Mag' with magnitude
feature_names <- gsub("Mag", "Magnitude", feature_names)
# remove hyphens from variable names
feature_names <- gsub("-mean\\(\\)-?", "Mean", feature_names)
feature_names <- gsub("-std\\(\\)-?", "StandardDeviation", feature_names)

names(data) <- feature_names

## Change activity names to be more descriptive
activity_names <- read.table("./UCI HAR Dataset/activity_labels.txt")
data <- left_join(data,activity_names, by = c("activity" = "V1"))
data <- data %>% mutate(activity = V2) %>% select(-V2)

## Create another tidy data set with the average of each variable for each
## activity and subject
tidy <- data %>% group_by(activity, subject) %>% summarise_each(funs(mean))
