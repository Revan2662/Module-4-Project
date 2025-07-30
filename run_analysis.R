# This script should do the following:
# 1. Merge the training and test sets into one large data set.
# 2. Extract only the measurements on the mean and standard deviation for each measurement.
# 3. Use descriptive activity names to name the activities in the data set.
# 4. Appropriately labels the data set with descriptive variable names.
# 5. From the data set in Step 4, create a second, independent tidy data set with teh average of each
# variable for each activity and each subject.

#General IDs
featureIDs <- read.table("UCI HAR Dataset/features.txt") 
#Grabs the IDs for each of the features that the data set uses.

activityIDs <- read.table("UCI HAR Dataset/activity_labels.txt")
#Grabs the IDs for each of the activities recorded in the data set.

#Training Data Sets
subjectTrainID <- read.table("UCI HAR Dataset/train/subject_train.txt")
#Grabs the IDs for each of the subjects in the training set.
  names(subjectTrainID) <- c("Subject ID")

xTrain <- read.table("UCI HAR Dataset/train/X_train.txt")
#Grabs the X training data for each of the subjects in the training set.
  names(xTrain) <- paste0("Data"," ", 1:ncol(xTrain))

yTrain <- read.table("UCI HAR Dataset/train/y_train.txt")
#Grabs the y training labels for each of the subjects in the training set.
  names(yTrain) <- c("Activity ID")

#Testing Data Sets
subjectTestID <- read.table("UCI HAR Dataset/test/subject_test.txt")
#Grabs the IDs for each of the subjects in the testing set.
  names(subjectTestID) <- c("Subject ID")

xTest <- read.table("UCI HAR Dataset/test/X_test.txt")
#Grabs the X testing data for each of the subjects in the training set.
  names(xTest) <- paste0("Data"," ", 1:ncol(xTest))

yTest <- read.table("UCI HAR Dataset/test/y_test.txt")
#Grabs the y testing labels for each of the subjects in the training set.
  names(yTest) <- c("Activity ID")



#Combining the data sets
testSet <- bind_cols(subjectTestID, bind_cols(yTest, xTest))
trainSet <- bind_cols(subjectTrainID, bind_cols(yTrain, xTrain))
fullSet <- bind_rows(testSet, trainSet)



#Transforming the data to gather the mean and stanard deviation of each measurement in the fullSet.
subjectIDList <- unique(fullSet[,"Subject ID"])
activityIDList <- unique(fullSet[,"Activity ID"])
IDcheck <- list()
meanDF <- data.frame("Subject" = character(), "Activity" = character(),"DataMean" = numeric(), "StandardDeviation" = numeric())

for (subject in subjectIDList) {
  subjectData <- filter(fullSet, fullSet$`Subject ID` == subject)
  
  subjectActivity <- unique(subjectData[,"Activity ID"])
  for (activity in subjectActivity) {
    if (activity %in% IDcheck){
      next
    }
    activityData <- filter(subjectData, subjectData$`Activity ID` == activity)
    activityMean <- mean(as.matrix(activityData[, 3:ncol(activityData)]), na.rm = TRUE)
    activitySD <- sd(as.matrix(activityData[, 3:ncol(activityData)]), na.rm = TRUE)
    IDcheck <- append(IDcheck, activity)
    newData <- data.frame("Subject" = as.character(subject), "Activity" = as.character(activity), "DataMean" = as.numeric(activityMean), "StandardDeviation" = as.numeric(activitySD))
    meanDF <- bind_rows(meanDF, newData)
    
  }
  
  IDcheck <- list()
  
}

#Orders each subject and activity ID before naming the activity from the activity list.
Results <- meanDF[order(as.numeric(meanDF$Subject), meanDF$Activity), ]
Results$Activity <- activityIDs$V2[match(Results$Activity, activityIDs$V1)]

#Opens the new data
View(Results)
write.table(Results, "Results.txt", row.names=FALSE)
