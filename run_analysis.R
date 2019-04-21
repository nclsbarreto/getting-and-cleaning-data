### GETTING AND CLEANING DATA PROJECT ###

# One of the most exciting areas in all of data science right now is wearable computing
# The data linked to from the course website represent data collected from the accelerometers 
# from the Samsung Galaxy S smartphone. A full description is available at the site where the 
# data was obtained:
  
# http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones

# Here are the data for the project:
#   
#   https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip

# You should create one R script called run_analysis.R

# Merges the training and the test sets to create one data set.
# Extracts only the measurements on the mean and standard deviation for each measurement.
# Uses descriptive activity names to name the activities in the data set
# Appropriately labels the data set with descriptive variable names.
# From the data set in step 4, creates a second, independent tidy 
# data set with the average of each variable for each activity and each subject.

#loadpackages
library(psych)
library(tidyverse)
library(stringr)

getwd()
dir("./UCI HAR Dataset")

### QUESTION 1 ---- merging the files together
#get feature names
dir("./UCI HAR Dataset")
features <- read.table(paste(getwd(), "/UCI HAR Dataset/features.txt", sep = "/"),
                       col.names = c("n","feature"))
#get activity labels 
activities <- read.table(paste(getwd(), "/UCI HAR Dataset/activity_labels.txt", sep = "/"),
                         col.names = c("code","activity"))
#get training
dir("./UCI HAR Dataset/train")
x_train <- read.table(paste(getwd(), "/UCI HAR Dataset/train/x_train.txt", sep = "/"), 
                      col.names = features$feature)
y_train <- read.table(paste(getwd(), "/UCI HAR Dataset/train/y_train.txt", sep = "/"), 
                      col.names = "code")
sub_train <- read.table(paste(getwd(), "/UCI HAR Dataset/train/subject_train.txt", sep = "/"),
                        col.names = "subject")
#get test
dir("./UCI HAR Dataset/test")
x_test <- read.table(paste(getwd(), "/UCI HAR Dataset/test/x_test.txt", sep = "/"), 
                     col.names = features$feature)
y_test <- read.table(paste(getwd(), "/UCI HAR Dataset/test/y_test.txt", sep = "/"), 
                     col.names = "code")
sub_test <- read.table(paste(getwd(), "/UCI HAR Dataset/test/subject_test.txt", sep = "/"), 
                       col.names = "subject")

#merge files
x <- rbind(x_train, x_test)
y <- rbind(y_train, y_test)
subj <- rbind(sub_train, sub_test)
db <- cbind(subj, x, y)

names(db)

### QUESTION 2 ---- getting only the mean and sd variables
tidy.db <- db %>% select(subject, code, contains("mean"), contains("std"))
names(tidy.db)

### QUESTION 3 ---- name the activities in the data set
str(tidy.db$code)
activities
tidy.db$code <- recode_factor(tidy.db$code,  
                                     '1' = "walking", 
                                     '2' = "walking_upstairs", 
                                     '3' = "walking_downstairs", 
                                     '4' = "sitting", 
                                     '5' = "standing", 
                                     '6' = "laying")

### QUESTION 4 ---- Make the variable names better
names(tidy.db)
names(tidy.db)[2] <- "activity"
names(tidy.db)<-gsub("Acc", "Accelerometer", names(tidy.db))
names(tidy.db)<-gsub("Gyro", "Gyroscope", names(tidy.db))
names(tidy.db)<-gsub("BodyBody", "Body", names(tidy.db))
names(tidy.db)<-gsub("Mag", "Magnitude", names(tidy.db))
names(tidy.db)<-gsub("tBody", "TimeBody", names(tidy.db))
names(tidy.db)<-gsub("^t", "Time", names(tidy.db))
names(tidy.db)<-gsub("^f", "Frequency", names(tidy.db))
names(tidy.db)<-gsub("-mean()", "Mean", names(tidy.db), ignore.case = TRUE)
names(tidy.db)<-gsub("-std()", "STD", names(tidy.db), ignore.case = TRUE)
names(tidy.db)<-gsub("-freq()", "Frequency", names(tidy.db), ignore.case = TRUE)
names(tidy.db)<-gsub("angle", "Angle", names(tidy.db))
names(tidy.db)<-gsub("gravity", "Gravity", names(tidy.db))
names(tidy.db)


### QUESTION 5 ---- From data in Q4, creates another data set: 
# the average of each variable for each activity and each subject.

?summarize_all
means.db <- tidy.db %>%
  group_by(activity, subject) %>%
  summarize_all(list(~mean(.)))

str(means.db)
names(means.db)
means.db
write.table(means.db, "Samsung Wearable Tech Measure Averages.txt", row.names = FALSE)
