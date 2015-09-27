# The purpose of this project is to demonstrate your ability to collect, work with, and clean a data set. The goal is to prepare tidy data that can be used for later analysis. You will be graded by your peers on a series of yes/no questions related to the project. You will be required to submit: 1) a tidy data set as described below, 2) a link to a Github repository with your script for performing the analysis, and 3) a code book that describes the variables, the data, and any transformations or work that you performed to clean up the data called CodeBook.md. You should also include a README.md in the repo with your scripts. This repo explains how all of the scripts work and how they are connected.  
# One of the most exciting areas in all of data science right now is wearable computing - see for example this article . Companies like Fitbit, Nike, and Jawbone Up are racing to develop the most advanced algorithms to attract new users. The data linked to from the course website represent data collected from the accelerometers from the Samsung Galaxy S smartphone. A full description is available at the site where the data was obtained: 
# http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones 
# Here are the data for the project: 
# https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip 
# You should create one R script called run_analysis.R that does the following. 
# Merges the training and the test sets to create one data set.
# Extracts only the measurements on the mean and standard deviation for each measurement. 
# Uses descriptive activity names to name the activities in the data set
# Appropriately labels the data set with descriptive variable names. 
# From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

###=== ACKNOWLEDGEMENTS- COMMUNITY TA - DAVID HOOD'S POST IN THE FORUM========#####

#setwd(Set Working Directory Here)
#--------------------------------------------------------------
# Download and place in "Analysis" folder
#---------------------------------------------------------------
if(!file.exists("./Analysis")){dir.create("./Analysis")}
fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(fileUrl,destfile="./Analysis/Dataset.zip")

#--------------------------------------------------------------
# Unzip File
#---------------------------------------------------------------

unzip(zipfile="./Analysis/Dataset.zip",exdir="./Analysis")
path <- file.path("./Analysis" , "UCI HAR Dataset")
#--------------------------------------------------------------
# HEADER|-----FEATURES.TXT-----|-------SUBJECT-------|-----LABELS-----|
#----------------------------------------------------------------------|
#     |------X_TRAIN.TXT----|--SUBJECT_TRAIN.TXT--|--Y_TRAIN.TXT---|-----> DECODE -ACTIVITY_LABELS.TXT
# DATA|------------------------------------------------------------|
#     |------X_TEST.TXT-----|--SUBJECT_TEST.TXT---|--Y_TEST.TXT----|-----> DECODE -ACTIVITY_LABELS.TXT
#---------------------------------------------------------------------
# READ LABELS FROM Y_TRAIN AND Y_TEST INTO DATASET
labelSetTrain<-read.table(file.path(path, "train" , "Y_train.txt" ),header = FALSE)
labelSetTEST<-read.table(file.path(path, "test" , "Y_test.txt" ),header = FALSE)

# READ SUBJECTS FROM SUBJECT_TRAIN AND SUBJECT_TEST INTO DATASET
subjectTrain<-read.table(file.path(path, "train" , "subject_train.txt" ),header = FALSE)
subjectTest<-read.table(file.path(path, "test" , "subject_test.txt" ),header = FALSE)

#READ THE MEASUREMENT DATA FROM X_TRAIN AND X_TEST
measureTrain<-read.table(file.path(path, "train" , "X_train.txt" ),header = FALSE)
measureTest<-read.table(file.path(path, "test" , "X_test.txt" ),header = FALSE)

#READ THE FEATURES HEADER ROW FROM FEATURS.TXT
measureHead<-read.table(file.path(path, "features.txt"),head=FALSE)


#MERGE DATASETS
labelSet<-rbind(labelSetTrain,labelSetTEST)
subject<-rbind(subjectTrain,subjectTest)
measure<-rbind(measureTrain,measureTest)

#ADD COLUMN NAMES
names(labelSet)<-c("activity")
names(subject)<-c("subject")
names(measure)<-measureHead$V2

# MERGE ALL DATA FRAMES INTO ONE
Data<-cbind(measure,subject,labelSet)

#---------------------------------------------------------------
#Extract only the columns with MEAN and SD a;long with Subjects and Activity

subsetHeader<-measureHead$V2[grep("mean\\(\\)|std\\(\\)", measureHead$V2)]

finalHeader<-c(as.character(subsetHeader), "subject", "activity" )
finalData<-subset(Data,select=finalHeader)

#Factorzie "activity" to descriptive names
decodeActivity<-read.table(file.path(path, "activity_labels.txt"),head=FALSE)
decodeActivity[,2] <- as.character(decodeActivity[,2])
finalData$activity <- factor(finalData$activity, levels = decodeActivity[,1], labels = decodeActivity[,2])

#APPROPRAITELY LABEL DATA WITH DESCRIPTIVE ACTIVITY NAMES
# replace t with Time
# replace Acc with Accelerometer
# replace f with Frequency
# replace gyro with Gyroscope
# replace mag with Maginitude
# replace bodybody with body
#replace Coeff with Coefficent

names(finalData)<-gsub("^t", "time", names(finalData))
names(finalData)<-gsub("^f", "frequency", names(finalData))
names(finalData)<-gsub("Acc", "Accelerometer", names(finalData))
names(finalData)<-gsub("Gyro", "Gyroscope", names(finalData))
names(finalData)<-gsub("Mag", "Magnitude", names(finalData))
names(finalData)<-gsub("BodyBody", "Body", names(finalData))


#Create second independent tidy data set with the average of each variable for each activity and each subject.
library(plyr)
Data<-aggregate(. ~subject + activity, finalData, mean)
Data<-Data[order(Data$subject,Data$activity),]
write.table(Data, file = file.path(path, "tidydata.txt"),row.name=FALSE)
