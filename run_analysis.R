library(dplyr)

##1. Download the zip file

print("Downloading files")

#Create directory
if (!file.exists("./data")) {
  dir.create("./data")
}

#Check if not even download
if (!file.exists("./data/har_dataset.zip")) {
  #Download
  zipFile<- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
  download.file(zipFile, destfile="./data/har_dataset.zip")
}

#Unzip
unzip(zipfile="./data/har_dataset.zip", exdir="./data")

##Read all data sets

#activity_labels
activity_labels <- read.table("./data/UCI HAR Dataset/activity_labels.txt", colClasses = "character")

#features
features <- read.table("./data/UCI HAR Dataset/features.txt")

#X_train
x_train <- read.table("./data/UCI HAR Dataset/train/X_train.txt", numerals = c("no.loss"))

#y_train
y_train <- read.table("./data/UCI HAR Dataset/train/y_train.txt")

#subject_train
subject_train <- read.table("./data/UCI HAR Dataset/train/subject_train.txt")

#X_test
x_test <- read.table("./data/UCI HAR Dataset/test/X_test.txt", numerals = c("no.loss"))

#y_test
y_test <- read.table("./data/UCI HAR Dataset/test/y_test.txt")

#subject_test
subject_test <- read.table("./data/UCI HAR Dataset/test/subject_test.txt")

print("Forming one data set")

names(activity_labels) <- c("id", "label")


names(y_train) <- c("activity")

y_train <- mutate(y_train, activity = activity_labels$label[activity])

names(y_test) <- c("activity")

y_test <- mutate(y_test, activity = activity_labels$label[activity])

#Change x_train and x_test variable names

names(features) <- c("id", "label")


names(x_train) <- features$label


names(x_test) <- features$label

#Bind x_train, y_train and subject_train

names(subject_train) <- c("subject")

yx_train <- cbind(y_train, x_train)

train <- cbind(subject_train, yx_train)

#Bind x_test, y_test and subject_test

names(subject_test) <- c("subject")

yx_test <- cbind(y_test, x_test)

test <- cbind(subject_test, yx_test)

human_activity <- rbind(train, test)

#Select the mean and standard deviation

human_activity_names <- names(human_activity)
selected_variables <- (
  (regexpr("^activity", human_activity_names) != -1) | 
  (regexpr("^subject", human_activity_names) != -1) | 
  (regexpr("mean\\(\\)", human_activity_names) != -1) | 
  (regexpr("std\\(\\)", human_activity_names) != -1)
)

human_activity <- human_activity[, selected_variables == TRUE]

##Forming tidy data set

# Step 5: From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
FinalData <- TidyData %>%
  group_by(subject, activity) %>%
  summarise_all(funs(mean))
write.table(FinalData, "FinalData.txt", row.name=FALSE)


# Final Check Stage
# Checking variable names
str(FinalData)

FinalData

tidy_data_set <- aggregate(. ~ activity + subject, data = human_activity, mean)

names(tidy_data_set)[c(3:dim(tidy_data_set)[2])] <- sub("(.)", "avg.\\1", names(tidy_data_set)[c(3:dim(tidy_data_set)[2])])
names(tidy_data_set) <- sub("-mean\\(\\)", "Mean", names(tidy_data_set))
names(tidy_data_set) <- sub("-std\\(\\)", "Std", names(tidy_data_set))
names(tidy_data_set) <- sub("-", "", names(tidy_data_set))

#Saving on txt file on the data folder
write.table(tidy_data_set, "./data/tidy_data_set.txt", row.name = FALSE)
