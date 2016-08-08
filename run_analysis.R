install.packages("plyr")
library(data.table)
library(dplyr)

# download the file into the data folder
if(!file.exists("./data")){dir.create("./data")}
fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(fileUrl,destfile = "./data/Dataset.zip", method = "curl")

# unzip the file
unzip(zipfile = "./data/Dataset.zip",exdir = "./data")

# show list of file names
data_path <- file.path("./data", "UCI HAR Dataset")
files <- list.files(data_path, recursive=TRUE)
files

# important files:
# test/subject_test.txt
# test/X_test.txt
# test/y_test.txt
# train/subject_train.txt
# train/X_train.txt
# train/y_train.txt

# read the features, subject, and activity files
features_test <- read.table(file.path(data_path, "test", "X_test.txt" ), header = FALSE)
features_train <- read.table(file.path(data_path, "train", "X_train.txt"), header = FALSE)

subject_test <- read.table(file.path(data_path, "test", "subject_test.txt"), header = FALSE)
subject_train <- read.table(file.path(data_path, "train", "subject_train.txt"), header = FALSE)

activity_test <- read.table(file.path(data_path, "test", "Y_test.txt" ), header = FALSE)
activity_train <- read.table(file.path(data_path, "train", "Y_train.txt"), header = FALSE)

### You should create one R script called "run_analysis.R" that does the following: ###

### STEP 1: Merges the training and the test sets to create one data set ###

features <- rbind(features_train, features_test)
subject <- rbind(subject_train, subject_test)
activity <- rbind(activity_train, activity_test)

names(subject) <- "subject"
names(activity) <- "activity"
features_names <- read.table(file.path(data_path, "features.txt"), head = FALSE)
names(features) <- features_names$V2

merged_data <- cbind(features, activity, subject)

### STEP 2: Extracts only the measurements on the mean and standard deviation for each measurement ###

mean_stdev <- grep(".*Mean.*|.*Std.*", names(merged_data), ignore.case=TRUE)
mean_stdev_columns <- c(mean_stdev, 562, 563)
dim(merged_data)

merged_data_extracted <- merged_data[,mean_stdev_columns]
dim(merged_data_extracted)
View(merged_data_extracted)

### STEP 3: Uses descriptive activity names to name the activities in the data set ###

activity_labels <- read.table(file.path(data_path, "activity_labels.txt"), header = FALSE)

merged_data_extracted$activity <- as.character(merged_data_extracted$activity)
for (i in 1:6){
  merged_data_extracted$activity[merged_data_extracted$activity == i] <- as.character(activity_labels[i,2])
}

merged_data_extracted$activity <- as.factor(merged_data_extracted$activity)

### STEP 4: Appropriately labels the data set with descriptive variable names ###

names(merged_data_extracted)

names(merged_data_extracted) <- gsub("Acc", "Accelerator", names(merged_data_extracted))
names(merged_data_extracted) <- gsub("Mag", "Magnitude", names(merged_data_extracted))
names(merged_data_extracted) <- gsub("Gyro", "Gyroscope", names(merged_data_extracted))
names(merged_data_extracted) <- gsub("^t", "Time", names(merged_data_extracted))
names(merged_data_extracted) <- gsub("^f", "Frequency", names(merged_data_extracted))
names(merged_data_extracted) <- gsub("BodyBody", "Body", names(merged_data_extracted))
names(merged_data_extracted) <- gsub("-mean()", "Mean", names(merged_data_extracted), ignore.case = TRUE)
names(merged_data_extracted) <- gsub("-std()", "StDev", names(merged_data_extracted), ignore.case = TRUE)
names(merged_data_extracted) <- gsub("-freq()", "Frequency", names(merged_data_extracted), ignore.case = TRUE)
names(merged_data_extracted) <- gsub("angle", "Angle", names(merged_data_extracted))
names(merged_data_extracted) <- gsub("gravity", "Gravity", names(merged_data_extracted))

### STEP 5: From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject ###

datatable_merged_data_extracted <- data.table(merged_data_extracted)

tidydata <- aggregate(. ~subject + activity, merged_data_extracted, mean)
tidydata <- tidydata[order(tidydata$subject,tidydata$activity),]
write.table(tidydata, file = "Tidy.txt", row.names = FALSE)

# create codebook
install.packages("memisc")
library(memisc)
sink("CodeBook.txt")
codebook(tidydata)
sink()