# This script does the following. 
# 1 Merges the training and the test sets to create one data set.
# 2 Extracts only the measurements on the mean and standard deviation for each measurement. 
# 3 Uses descriptive activity names to name the activities in the data set
# 4 Appropriately labels the data set with descriptive variable names. 
# 5 From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.


library(plyr)

if (!file.exists("UCI HAR Dataset")) {
  zipfile="UCI_HAR_data.zip"
  if (!file.exists(zipfile))
    download.file("https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip", dest=zipfile, method="curl")
  unzip(zipfile)
}

read <- function(x,t) {
  filename <- paste0("UCI HAR Dataset/", t, "/", x, "_", t, ".txt")
  read.table(filename)
}

print("load the merged measurement dataset") 

#load the merged measurement dataset
if (!exists("X"))
  X <- rbind(read("X", "train"), read("X", "test"))

#get only the interesting columns and name them appropriately

features <- read.table("UCI HAR Dataset/features.txt")
col_mean <- sapply(features[,2], function(x) grepl("mean()", x, fixed=TRUE))
col_std <- sapply(features[,2], function(x) grepl("std()", x, fixed=TRUE))
columns <- col_mean | col_std
x<- X[,columns]
colnames(x) <- features[columns, 2]

print("load the merged activity and subject dataset")

#load the merged activity dataset, put appropriate names for the activity values

y <- rbind(read("y", "train"), read("y", "test"))

colnames(y) <- c("activity")
y$activity[y$activity == 1] = "WALKING"
y$activity[y$activity == 2] = "WALKING_UPSTAIRS"
y$activity[y$activity == 3] = "WALKING_DOWNSTAIRS"
y$activity[y$activity == 4] = "SITTING"
y$activity[y$activity == 5] = "STANDING"
y$activity[y$activity == 6] = "LAYING"


#load the merged subject dataset
subject <- rbind(read("subject", "train"), read("subject", "test"))

colnames(subject) <- c("subject")

print("combine the datasets")

# make a single tidy data set out of the 3 data sets
tidy <- cbind(subject, y, x)
write.csv(tidy, "tidy.csv")

print("create the second dataset")

# creates a second, independent tidy data set with the average of each variable
# for each activity and each subject.
average <- ddply(tidy, .(subject, activity), function(x) colMeans(x[,3:length(names(x))]))
write.table(average, "average.txt", row.name=FALSE)

