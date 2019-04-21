This file contains limited details of the code in the run_analysis.R script
The run_analysis.R script performs the 5 steps required of the peer-reviewed-assignment.

Download the dataset
  Dataset downloaded and extracted as a zip folder called UCI HAR Dataset
    https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip

  Further details on the data can be found at website below:
    http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones

Each data file (.txt) was loaded as follows 
  features <- features.txt : 561 rows, 2 columns 
    File contians the names of the variables in the data set
  activities <- activity_labels.txt : 6 rows, 2 columns 
    File contains list of activities performed measurements were taken and the codes (labels)
  subject_test <- test/subject_test.txt : 2947 rows, 1 column 
    File contains subject IDs for test data 
  x_test <- test/X_test.txt : 2947 rows, 561 columns 
    File contains scores on all features for the test data
  y_test <- test/y_test.txt : 2947 rows, 1 columns 
    File contains test data of activities’ code 
  subject_train <- test/subject_train.txt : 7352 rows, 1 column 
    File contains subject IDs for train data 
  x_train <- test/X_train.txt : 7352 rows, 561 columns 
    File contains scores on all features for the train data
   y_train <- test/y_train.txt : 7352 rows, 1 columns 
    File contains train data of activities’ code 

Merges the training, test, and subject IDs to create one data set
  x (10299 rows, 561 columns)
    created by merging x_train and x_test using rbind() function
  y (10299 rows, 1 column) 
    created by merging y_train and y_test using rbind() function
  Subject (10299 rows, 1 column) 
    created by merging subject_train and subject_test using rbind() function
  db (10299 rows, 563 column) 
    created by merging Subject, Y and X using cbind() function
      - it was assumed that the rows across all three data sets were in the same order
      
Extracts only the measurements on the mean and standard deviation for each measurement
  tidy.db (10299 rows, 88 columns) 
    created by subsetting db, 
    selecting only columns: subject, code and 
      the mean and standard deviation (std) of each measurement

Uses descriptive activity names to name the activities in the data set
  Activty code numbers were replaced with activity names taken from the second column in the activities dataset

Appropriately labels the data set with descriptive variable names
  code column in tidy.db renamed into activities
  in column names all of the following were done:
    All Acc replaced by Accelerometer
    All Gyro replaced by Gyroscope
    All BodyBody replaced by Body
    All Mag replaced by Magnitude
    All starting "f"s replaced by Frequency
    All starting "t"s replaced by Time

Create a means data set with the average of each variable for each activity and each subject
  means.db (180 rows, 88 columns) created by:
    1) grouping tidy.db by activities and subject ID
    2) taking the average (using summarizing_all) of each variable.

Export FinalData into Samsung Wearable Tech Measure Averages.txt file.
