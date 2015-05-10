# prepare the data files: download and unzip
if(!file.exists("getdata-projectfiles-UCI HAR Dataset.zip")){
  download.file("https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip", 
                destfile = "getdata-projectfiles-UCI HAR Dataset.zip")
}
if(!file.exists("./UCI HAR Dataset")){
  unzip("getdata-projectfiles-UCI HAR Dataset.zip")
}

# 1. Merges the training and the test sets to create one data set.
#    according to the readme.txt in dataset, these files are processing:
#    - 'train/X_train.txt': Training set.
#    - 'train/y_train.txt': Training labels.
#    - 'test/X_test.txt': Test set.
#    - 'test/y_test.txt': Test labels.

# get column names
names_561_feature <- read.csv(file = "./UCI HAR Dataset/features.txt", header = F, sep = " ")
names_561_feature$V2 <- make.names(names_561_feature$V2)

# the fixed-width file contains leading whitespace and double whitespace separator
## x_test <- read.csv(file = "./UCI HAR Dataset/test/X_test.txt", header = F, sep = " ")
x_test0 <- readLines("./UCI HAR Dataset/test/X_test.txt" )
x_test0 <- strsplit(x_test0, split = "\n")
x_test0 <- gsub("^  *", "", x_test0)
x_test0 <- gsub("  *", " ", x_test0)

x_test1 <- strsplit(x_test0, split = " ")
x_test <- matrix(as.numeric(unlist(x_test1)), ncol = 561, byrow = T)
x_test <- data.frame(x_test)
colnames(x_test) <- names_561_feature$V2

y_test <- read.csv(file = "./UCI HAR Dataset/test/y_test.txt", header = F, sep = " ")

# read train data
x_train0 <- readLines("./UCI HAR Dataset/train/X_train.txt" )
x_train0 <- strsplit(x_train0, split = "\n")
x_train0 <- gsub("^  *", "", x_train0)
x_train0 <- gsub("  *", " ", x_train0)

x_train1 <- strsplit(x_train0, split = " ")
x_train <- matrix(as.numeric(unlist(x_train1)), ncol = 561, byrow = T)
x_train <- data.frame(x_train)
colnames(x_train) <- names_561_feature$V2

y_train <- read.csv(file = "./UCI HAR Dataset/train/y_train.txt", header = F, sep = " ")

# merge the datas of train+test AND
# 2 Extracts only the measurements on the mean and standard deviation for each measurement.
y_label <- rbind(y_test,y_train)
x_data <- rbind(x_test, x_train)
col_mean_std <- c("tBodyAcc.mean...X", "tBodyAcc.mean...Y", "tBodyAcc.mean...Z",
                  "tBodyAcc.std...X", "tBodyAcc.std...Y", "tBodyAcc.std...Z",
                  "tGravityAcc.mean...X", "tGravityAcc.mean...Y", "tGravityAcc.mean...Z",
                  "tGravityAcc.std...X", "tGravityAcc.std...Y", "tGravityAcc.std...Z",
                  "tBodyAccJerk.mean...X", "tBodyAccJerk.mean...Y", "tBodyAccJerk.mean...Z",
                  "tBodyAccJerk.std...X", "tBodyAccJerk.std...Y", "tBodyAccJerk.std...Z",
                  "tBodyGyro.mean...X", "tBodyGyro.mean...Y", "tBodyGyro.mean...Z",
                  "tBodyGyro.std...X", "tBodyGyro.std...Y", "tBodyGyro.std...Z",
                  "tBodyGyroJerk.mean...X", "tBodyGyroJerk.mean...Y",                  
                  "tBodyGyroJerk.mean...Z", "tBodyGyroJerk.std...X",
                  "tBodyGyroJerk.std...Y", "tBodyGyroJerk.std...Z", "tBodyAccMag.mean..",
                  "tBodyAccMag.std..", "tGravityAccMag.mean..", "tGravityAccMag.std..",
                  "tBodyAccJerkMag.mean..", "tBodyAccJerkMag.std..",
                  "tBodyGyroMag.mean..", "tBodyGyroMag.std..", "tBodyGyroJerkMag.mean..",
                  "tBodyGyroJerkMag.std..", "fBodyAcc.mean...X", "fBodyAcc.mean...Y",
                  "fBodyAcc.mean...Z", "fBodyAcc.std...X", "fBodyAcc.std...Y",
                  "fBodyAcc.std...Z", "fBodyAcc.meanFreq...X", "fBodyAcc.meanFreq...Y",
                  "fBodyAcc.meanFreq...Z", "fBodyAccJerk.mean...X", "fBodyAccJerk.mean...Y",
                  "fBodyAccJerk.mean...Z", "fBodyAccJerk.std...X", "fBodyAccJerk.std...Y",
                  "fBodyAccJerk.std...Z", "fBodyAccJerk.meanFreq...X",
                  "fBodyAccJerk.meanFreq...Y", "fBodyAccJerk.meanFreq...Z",
                  "fBodyGyro.mean...X", "fBodyGyro.mean...Y", "fBodyGyro.mean...Z",
                  "fBodyGyro.std...X", "fBodyGyro.std...Y", "fBodyGyro.std...Z",
                  "fBodyGyro.meanFreq...X", "fBodyGyro.meanFreq...Y",
                  "fBodyGyro.meanFreq...Z", "fBodyAccMag.mean..", "fBodyAccMag.std..",
                  "fBodyAccMag.meanFreq..", "fBodyBodyAccJerkMag.mean..",
                  "fBodyBodyAccJerkMag.std..", "fBodyBodyAccJerkMag.meanFreq..",
                  "fBodyBodyGyroMag.mean..", "fBodyBodyGyroMag.std..",
                  "fBodyBodyGyroMag.meanFreq..", "fBodyBodyGyroJerkMag.mean..",
                  "fBodyBodyGyroJerkMag.std..", "fBodyBodyGyroJerkMag.meanFreq..",
                  "angle.tBodyAccMean.gravity.", "angle.tBodyAccJerkMean..gravityMean.",
                  "angle.tBodyGyroMean.gravityMean.", "angle.tBodyGyroJerkMean.gravityMean.",
                  "angle.X.gravityMean.", "angle.Y.gravityMean.", "angle.Z.gravityMean.")
x_data_mean_std <- x_data[,col_mean_std]

# 3. Uses descriptive activity names to name the activities in the data set
# already done by set column name with names_561_feature$v2

# 4. Appropriately labels the data set with descriptive variable names. 
activity_labels <- read.csv(file = "./UCI HAR Dataset/activity_labels.txt", header = F, sep = " ")
record_label <- y_label
# create an ID column to keep labels in order
record_label$id <- 1:nrow(record_label)
# merge original label with feature description
record_label <- merge(x=record_label, y=activity_labels, by = "V1")
record_label <- record_label[order(record_label$id), ]
# assign labels to records
# #x_data_mean_std$label_id <- y_label$V1
x_data_mean_std$label <- record_label$V2


# 5. From the data set in step 4, creates a second,
#   independent tidy data set with the average of 
#   each variable for each activity and each subject.
subject_test <- read.csv(file = "./UCI HAR Dataset/test/subject_test.txt", header = F, sep = " ")
subject_train <- read.csv(file = "./UCI HAR Dataset/train/subject_train.txt", header = F, sep = " ")
subject_data <- rbind(subject_test, subject_train)
data_2 <- cbind(subject_data, x_data_mean_std)
avg_act_subject <- aggregate(. ~ V1 + label, data=data_2, mean, na.action=na.omit)

# generate the tidy data for #5
write.table(avg_act_subject, file="avg_act_subject.txt", row.name=FALSE)