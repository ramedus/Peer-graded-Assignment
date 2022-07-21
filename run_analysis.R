#R Coursera Final Project 

# 1 Merges the training and the test sets to create one data set.
install.packages("data.table")
install.packages("dplyr")
library(data.table)
library(dplyr)

getwd()

# read the tables features.txt and activity_labels.txt from the UCI HAR Dataset
featureNames <- read.table("features.txt", header = FALSE)
activityLabels <- read.table("activity_labels.txt", header = FALSE)

# read training data
subjectTrain <- read.table("train/subject_train.txt", header = FALSE)
activityTrain <- read.table("train/y_train.txt", header = FALSE)
featuresTrain <- read.table("train/X_train.txt", header = FALSE)

# read test data
subjectTest <- read.table("test/subject_test.txt", header = FALSE)
activityTest <- read.table("test/y_test.txt", header = FALSE)
featuresTest <- read.table("test/X_test.txt", header = FALSE)

# we combine the training and test data sets corresponding to subject, activity, and features. 

subject <- rbind(subjectTrain, subjectTest)
activity <- rbind(activityTrain, activityTest)
features <- rbind(featuresTrain, featuresTest)

# The columns in the features data set can be named from the metadata in featureNames

colnames(features) <- t(featureNames[2])

# Merge the data
colnames(activity) <- "Activity"
colnames(subject) <- "Subject"
completeData <- cbind(features,activity,subject)


# 2 Extracts only the measurements on the mean and standard deviation for each measurement

columnsWithMeanSTD <- grep(".*Mean.*|.*Std.*", names(completeData), ignore.case=TRUE)

# Add activity and subject columns to the list and look at the dimension of completeData

requiredColumns <- c(columnsWithMeanSTD, 562, 563)
dim(completeData)

## [1] 10299   563
# We create extractedData with the selected columns in requiredColumns. 

extractedData <- completeData[,requiredColumns]
dim(extractedData)


# 3 Uses descriptive activity names to name the activities in the data set

extractedData$Activity <- as.character(extractedData$Activity)
for (i in 1:6){
  extractedData$Activity[extractedData$Activity == i] <- as.character(activityLabels[i,2])
}
extractedData$Activity <- as.factor(extractedData$Activity)


# 4 Appropriately labels the data set with descriptive variable names. 

names(extractedData)

names(extractedData)<-gsub("Acc", "Accelerometer", names(extractedData))
names(extractedData)<-gsub("Gyro", "Gyroscope", names(extractedData))
names(extractedData)<-gsub("BodyBody", "Body", names(extractedData))
names(extractedData)<-gsub("Mag", "Magnitude", names(extractedData))
names(extractedData)<-gsub("^t", "Time", names(extractedData))
names(extractedData)<-gsub("^f", "Frequency", names(extractedData))
names(extractedData)<-gsub("tBody", "TimeBody", names(extractedData))
names(extractedData)<-gsub("-mean()", "Mean", names(extractedData), ignore.case = TRUE)
names(extractedData)<-gsub("-std()", "STD", names(extractedData), ignore.case = TRUE)
names(extractedData)<-gsub("-freq()", "Frequency", names(extractedData), ignore.case = TRUE)
names(extractedData)<-gsub("angle", "Angle", names(extractedData))
names(extractedData)<-gsub("gravity", "Gravity", names(extractedData))

names(extractedData)


# 5 From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.


extractedData$Subject <- as.factor(extractedData$Subject)
extractedData <- data.table(extractedData)


tidyData <- aggregate(. ~Subject + Activity, extractedData, mean)
tidyData <- tidyData[order(tidyData$Sub, tidyData$Activity),]
write.table(tidyData, file = "Tidydata.txt", row.names = FALSE)
