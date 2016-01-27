setwd("/Users/donalp/Dropbox/3-Work/Training/Software stuff/Coding/R/DataScience/Coursera/Getting-cleaning-data/Galaxy-Assignment")
library(plyr)
library(dplyr)
library(reshape2)

## Load in data
  if (!file.exists("./data")) {
    dir.create("./data")
  }
  
  if (!file.exists("./data/UCI HAR Dataset.zip")) {
    download.file(
      url="https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip",
      method="curl",
      destfile="./data/UCI HAR Dataset.zip")
    unzip("./data/UCI HAR Dataset.zip", exdir="./data")
  }

  ## Get List of all features.
    features<-read.table("data/UCI HAR Dataset/features.txt")
    # The mean and std subset
    targetFeatures<-filter(features, grepl("mean\\(\\)|std\\(\\)",features$V2))
  
  ## Get activity Labels
    activityLabels<-read.table("data/UCI HAR Dataset/activity_labels.txt")
  
  ## Get the training data
    trainingSet<-read.table("data/UCI HAR Dataset/train/X_train.txt")
    trainingSubjects<-read.table("data/UCI HAR Dataset/train/subject_train.txt")
    trainingLabels<-read.table("data/UCI HAR Dataset/train/y_train.txt")
    trainingLabels<-merge(trainingLabels, activityLabels, by.x="V1", by.y="V1") # match activity numbers to names
    
    # Combine all training data
    fullTraining<-cbind(trainingSubjects, trainingLabels, trainingSet)
    fullTraining<-fullTraining[,-2] # remove redundant column
  
  ## Get the test data
    testSet<-read.table("data/UCI HAR Dataset/test/X_test.txt")
    testSubjects<-read.table("data/UCI HAR Dataset/test/subject_test.txt")
    testLabels<-read.table("data/UCI HAR Dataset/test/y_test.txt")
    testLabels<-merge(testLabels, activityLabels, by.x="V1", by.y="V1") # match activity numbers to names
      
    # Combine all test data
    fullTest<-cbind(testSubjects, testLabels, testSet)
    fullTest<-fullTest[,-2] # remove redundant column

## Merge the training and the test sets to create one data set.
  fullData<-rbind(fullTraining, fullTest)

# Extracts only the measurements on the mean and standard deviation for each measurement.
  fullData<-select(fullData, 1, 2, targetFeatures$V1+2)
  names(fullData)<-c("subject","activity", as.character(targetFeatures$V2))

# Clean up the workspace
  rm(list=c("features","fullTest","fullTraining","targetFeatures",
            "testLabels","testSet","testSubjects","trainingLabels",
            "trainingSet","trainingSubjects","activityLabels"))
  
# Appropriately labels the data set with descriptive variable names (good luck with that!)
  currentNames<-names(fullData)
  currentNames<-sub("Acc", "Accelerometer", currentNames)
  currentNames<-sub("Gyro", "Gyroscope", currentNames)
  currentNames<-sub("Mag", "Magnitude", currentNames)
  currentNames<-sub("-mean\\(\\)", "Mean", currentNames)
  currentNames<-sub("-std\\(\\)", "Stdev", currentNames)
  currentNames<-gsub("-", "", currentNames)
  names(fullData)<-currentNames

# Create a second, independent tidy data set with
# the average of each variable for each activity and each subject.
  tidyData <- fullData %>% melt(id.vars=c("subject","activity")) %>% 
    group_by(subject, activity, variable) %>% 
    summarise(value=mean(value)) %>%
    arrange(subject, activity, variable)
    tidyData$variable<-paste0( "mean",tidyData$variable )  

# Output the data
  write.csv(fullData, file="output/fullData.csv")
  write.csv(tidyData, file="output/tidyData.csv")
