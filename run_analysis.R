#This is R program will help us to do the data cleansing and processing of Human Activity Recognition DataSet.
# Finaly we will get a tidy set saved as UCIHARtidy.txt
library(plyr)
#UCI HAR Dataset DIR
uci_har_dir <- "UCI HAR Dataset"

#Activity Label
activity_labels <- read.table(paste(uci_har_dir, "/activity_labels.txt", sep=""), 
                sep=" ", col.names=c("activityId","activityname"), strip.white=T)
#Features
features<-read.table(paste(uci_har_dir, "/features.txt", sep=""), 
                col.names = c("featureID","featureName"), strip.white=T)

#Read Train Data & Subjects
subjectTrain <- read.table(paste(uci_har_dir, "/train/subject_train.txt", sep=""), 
                          sep=" ", col.names=c("Subject"), strip.white=T)
trainActivity <- read.table(paste(uci_har_dir, "/train/y_train.txt", sep=""), 
                       sep=" ", col.names = c("activityId"), strip.white=T)
trainSet <- read.table(paste(uci_har_dir, "/train/X_train.txt", sep=""))


# Read Test Data & Subjects
subjectTest <- read.table(paste(uci_har_dir, "/test/subject_test.txt", sep=""), 
                                  sep=" ", col.names=c("Subject"), strip.white=T)
testActivity <- read.table(paste(uci_har_dir, "/test/y_test.txt", sep=""), 
                                 sep=" ", col.names = c("activityId"), strip.white=T)
testSet <- read.table(paste(uci_har_dir, "/test/X_test.txt", sep=""))


#Merge Train and Test Subjects
fullSubjects <- rbind(subjectTrain, subjectTest)
#Merge Train and Test Activity Labels
fullActivities <- rbind(trainActivity, testActivity)
#Merge Train and Test Activity Sets
fullSets <- rbind (trainSet, testSet)
#Set the Column Names for ActivitySet
names(fullSets) <- features[,2]

#Assign the Activity Label by Activity ID
fullActivities <- join(fullActivities, activity_labels, by = "activityId", match = "first")

#Select the Mean and STD cols
selectedMeanCols <- sapply(features[,2], function(x) grepl("mean()",x, fixed=T))
selectedStdCols <- sapply(features[,2], function(x) grepl("std()",x, fixed=T))
#Select only the Mean and STD measurements from Activity Sets
fullSets <- fullSets[, (selectedMeanCols|selectedStdCols)]

#Merge all the columns
fullData <- cbind(Subject=fullSubjects,Activity=fullActivities[,2], fullSets)

#Calculate the Mean for all the measurements
tdy<-ddply(fullData, c("Subject","Activity"), numcolwise(mean))
#Write the tidy data into file
write.csv(tdy, "UCIHARtidy.txt", row.names=FALSE)

