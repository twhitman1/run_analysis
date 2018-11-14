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


##################
mypath = "./Class3_GettingAndCleaningData/data/UCI_HAR_Dataset"
setwd(mypath)

# Create list of the files for reference
    txtFileNames = list.files(dir(), pattern = "*.txt", recursive = TRUE)
    
# Load relevant libraries
    library(dplyr)
    library(tidyr)
    

####  BUILD SINGLE DATAFRAME
  ####  Train DataFrame
    #### First, x_train data
      xtrdf <- read.table("train/X_train.txt", sep = "")
      
      ##  Extract y_train variable names into 'ytrNames'
      VariableNames <- read.table("features.txt", sep = "")
      VariableNames <- as.character(VariableNames[,2])
      
      ##  Apply variable names to the df
      names(xtrdf) <- VariableNames
      
    #### Second, y_train data
      ytrdf <- read.table("train/y_train.txt", sep = "")
      names(ytrdf) <- "Activities"
      
      ##  Extract activity names
      ActNames <- read.table("activity_labels.txt", sep="")
      ActNames <- as.character(ActNames[,2])
      ActMap <- as.data.frame(cbind(c(1:6), ActNames))
      colnames(ActMap)[colnames(ActMap)=="V2"] <- "ActCode"
      
      ##  Set y_train data to levels
      levels(ytrdf) <- ActNames
      
    ####  third, subject_train data into subTrain
      subTrain <- read.table("train/subject_train.txt", sep = "")
      names(subTrain) <- "subject"
      
    ####  Fourth, bind the above xtrdf with the ytr factor and subTrain factor
      train <- cbind(subTrain, ytrdf, xtrdf)
      train$subject <- as.factor(train$subject)
      train$Activities <- as.character(train$Activities)
      
  ####  Test DataFrame
    #### First, x_test data
      xtest <- read.table("test/X_test.txt", sep = "")
      
      ##  Apply variable names to the df
      names(xtest) <- VariableNames
      
    #### Second, y_test data
      ytest <- read.table("test/y_test.txt", sep = "")
      names(ytest) <- "Activities"
      
      ##  Set y_train data to levels
      levels(ytest) <- ActNames
      
    ####  third, subject_train data into subTrain
      subTest <- read.table("test/subject_test.txt", sep = "")
      names(subTest) <- "subject"
      
    ####  Fourth, bind the above xtest with the ytest factor and subTest factor
      test <- cbind(subTest, ytest, xtest)
      test$subject <- as.factor(test$subject)
      test$Activities <- as.character(test$Activities)
      
  #### Merge test and train data
      ## add column to identify data from training cohort (cohort 1) vs. test cohort (cohort 2)
      train$cohort <- as.character("1")
      test$cohort <- as.character("2")
      mergedDF <- rbind(train, test, stringsAsFactors = FALSE)
      
      ##  remove columns with duplicated names (none are relevant to mean or std)
      mergedDF <- mergedDF[, !duplicated(colnames(mergedDF))]
      
      ##  select cohort, subject and activities columns as well as variable columns with "mean" or "std"
      ##  in character string of column header
      DF1 <- select(mergedDF, cohort, subject, Activities, contains("mean"), contains("std"))
      # remove columns with "meanfreq" in the name
      DF1 <- select(DF1, -(contains("meanfreq")))
          
          # update activity codes to corresponding activity descriptive names
          DF1$Activities[DF1$Activities=="1"] <- "WALKING"
          DF1$Activities[DF1$Activities=="2"] <- "WALKING_UPSTAIRS"
          DF1$Activities[DF1$Activities=="3"] <- "WALKING_DOWNSTAIRS"
          DF1$Activities[DF1$Activities=="4"] <- "SITTING"
          DF1$Activities[DF1$Activities=="5"] <- "STANDING"
          DF1$Activities[DF1$Activities=="6"] <- "LAYING"
          
          # use descriptive cohort names
          DF1$cohort[DF1$cohort=="1"] <- "training cohort"
          DF1$cohort[DF1$cohort=="2"] <- "test cohort"
          
          # update variable (column) labels to make more legible
          names(DF1) <- gsub("^t", "Time:", names(DF1))
          names(DF1) <- gsub("^f", "FFT:", names(DF1))
          names(DF1) <- gsub("mean()", "mean.value", names(DF1), fixed = TRUE)
          names(DF1) <- gsub("gravityMean", "gravity.mean.value", names(DF1))
          names(DF1) <- gsub("std()", "standard.deviation", names(DF1), fixed = TRUE)
          names(DF1) <- gsub("Body", "Body.", names(DF1))
          names(DF1) <- gsub("Acc", "Acceleration", names(DF1))
          names(DF1) <- gsub("GravityA", "Gravity.A", names(DF1))
          names(DF1) <- gsub("nJ", "n.J", names(DF1))
          names(DF1) <- gsub("GyroJerk", "Gyro.Jerk", names(DF1))
          names(DF1) <- gsub("Mag", ".Magnitude", names(DF1))
          names(DF1) <- gsub("Body.Body", "Body", names(DF1))
          names(DF1) <- gsub("Gyro", "Gyroscope", names(DF1))
          
## Create Final Data Frame (FDF), grouping by subject and activities, and calculating mean on all 
          ## variables based on this grouping.
      FDF <- DF1 %>%
        group_by(subject, Activities) %>%
        summarise_at(.vars = 4:76, funs(mean))
      
##  Create txt file output of the final data frame
      write.table(FDF, "FinalData.txt", row.names = FALSE)