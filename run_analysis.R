## Getting and Cleaning Data Course Project

## Merges the training and the test sets to create one data set.
## Extracts only the measurements on the mean and standard deviation for each measurement.
## Uses descriptive activity names to name the activities in the data set
## Appropriately labels the data set with descriptive variable names.
## From the data set in step 4, creates a second, independent tidy data set with the average 
## of each variable for each activity and each subject.

## Description of data set: http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones
## Data set: https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip

library(reshape2)

## My working directory
setwd("D:/Dropbox/DataScience/Getting and cleaning data/Project")

## Download data
if(!file.exists("./data")){dir.create("./data")} ## Creates data directory it it doesn't exists
fileUrl <- 'https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip' ## Data url
download.file(fileUrl,destfile = "./data/dataset.zip") ## Downloading the data set
unzip("./data/dataset.zip", files = NULL, list = FALSE, overwrite = TRUE,
      junkpaths = FALSE, exdir = "./data", unzip = "internal",
      setTimes = FALSE) ## Unzip the data set

## Read activity labels and features
activity_labels <- read.table("./data/UCI HAR Dataset/activity_labels.txt")
activity_labels[,2] <- as.character(activity_labels[,2])
features <- read.table("./data/UCI HAR Dataset/features.txt")
features[,2] <- as.character(features[,2])

## Search for mean and std
features_Mean_STD <- grep(".*mean.*|.*std.*", features[,2])
features_names <- features[features_Mean_STD,2] ## Second column -> names

## Names are writen with -mean/-std and -() or very abreviated names which could be a bad practice to name variables
## Changing names
features_names <- gsub('-mean','Mean',features_names)
features_names <- gsub('-std','Std',features_names)
features_names <- gsub('[-()]','',features_names)
features_names <- gsub("^(t)","time",features_names)
features_names <- gsub("^(f)","freq",features_names)

## Load data

## Subjects
subjectTrain <- read.table("./data/UCI HAR Dataset/train/subject_train.txt")
subjectTest <- read.table("./data/UCI HAR Dataset/test/subject_test.txt")

## DataSet
XTrain <- read.table("./data/UCI HAR Dataset/train/X_train.txt")[features_Mean_STD] # Choose from the data set those features related with mean and std
XTest <- read.table("./data/UCI HAR Dataset/test/X_test.txt")[features_Mean_STD]

## Activities
YTrain <- read.table("./data/UCI HAR Dataset/train/y_train.txt")
YTest <- read.table("./data/UCI HAR Dataset/test/y_test.txt")

DataTrain <- cbind(subjectTrain,YTrain,XTrain)
DataTest <- cbind(subjectTest,YTest,XTest)
## Join data columns
DataSet_1<-rbind(DataTrain,DataTest)

## Name columns
colnames(DataSet_1) <- c("subject", "activity", features_names)
DataSet_1 <- DataSet_1[!is.na(colnames(DataSet_1))]
## Set factors
DataSet_1$activity <- factor(DataSet_1$activity, levels = activity_labels[,1], labels = activity_labels[,2])
DataSet_1$subject <- as.factor(DataSet_1$subject)

## Subject and activity melting
DataSet_1_melted <- melt(DataSet_1, id = c("subject", "activity"))

## Cast mean
DataSet_1_mean <- dcast(DataSet_1_melted, subject + activity ~ variable, mean)

## Write tidy data set with the average of each variable
write.table(DataSet_1_mean,"tidy.txt",row.names = FALSE)
