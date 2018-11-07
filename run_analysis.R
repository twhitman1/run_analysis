##################################
##########Class 3 Final Assignment
##################################

##  Here are the data for the project:
##  https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip

##  You should create one R script called run_analysis.R that does the following.
    ##  1. Merges the training and the test sets to create one data set.
    ##  2. Extracts only the measurements on the mean and standard deviation for each measurement.
    ##  3. Uses descriptive activity names to name the activities in the data set
    ##  4. Appropriately labels the data set with descriptive variable names.
    ##  5. From the data set in step 4, creates a second, independent tidy data set with the 
            ##  average of each variable for each activity and each subject.

if(!file.exists("./Class3_GettingAndCleaningData/data")){dir.create("./Class3_GettingAndCleaningData/data")}
zipurl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(zipurl,"./Class3_GettingAndCleaningData/data/wearables.zip", exdir = "./CLass3_GettingAndCleaningData/data")

setwd("./Class3_GettingAndCleaningData/data/")