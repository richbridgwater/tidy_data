#This file is to create tidy_data.txt file from the Test and train data
#In R directory was set as source directory under which there was a directory names 'UCI HAR Dataset'

# Loading activity labels
labels_activity <- read.table("./UCI HAR Dataset/activity_labels.txt")
labels_activity <- labels_activity[,2]
# below is to print labels
#labels_activity

# Loading data column names
features <- read.table("./UCI HAR Dataset/features.txt")[,2]
#print features
#features

# Grepping only the measurements on the mean and standard deviation for each measurement.
extract_features <- grepl("mean|std", features)
#extract_features

# Loading and processing X_test & y_test data.
test_x <- read.table("./UCI HAR Dataset/test/X_test.txt")
test_y <- read.table("./UCI HAR Dataset/test/y_test.txt")
test_subject <- read.table("./UCI HAR Dataset/test/subject_test.txt")

names(test_x) = features

# Extracting measurements mean and standard deviation for each measurement.
test_x = test_x[,extract_features]


# Loading activity labels
test_y[,2] = labels_activity[test_y[,1]]
names(test_y) = c("Activity_ID", "Activity_Label")
names(test_subject) = "Subject"

# Bind data
#install.packages("data.table")
library(data.table)
test_data <- cbind(as.data.table(test_subject), test_y, test_x)

# Load and process X_train & y_train data.
train_x <- read.table("./UCI HAR Dataset/train/X_train.txt")
train_y <- read.table("./UCI HAR Dataset/train/y_train.txt")

train_subject <- read.table("./UCI HAR Dataset/train/subject_train.txt")

names(train_x) = features

# Extract only the measurements on the mean and standard deviation for each measurement.
train_x = train_x[,extract_features]

# Load activity data
train_y[,2] = labels_activity[train_y[,1]]
names(train_y) = c("Activity_ID", "Activity_Label")
names(train_subject) = "Subject"

# Binding data
train_data <- cbind(as.data.table(train_subject), train_y, train_x)
# Merging test and train data
data = rbind(test_data, train_data, fill=TRUE)

id_labels   = c("Subject", "Activity_ID", "Activity_Label")
data_labels = setdiff(colnames(data), id_labels)
#install.packages("reshape"); 
library(reshape)

#melting data so that each row is a unique id-variable combination.

melt_data      = melt(data, id = id_labels, measure.vars = data_labels)

#Casting again the melted data into 'subject and activity labels' with mean
tidy_data   = cast(melt_data, Subject + Activity_Label ~ variable, mean)

write.table(tidy_data, file = "./tidy_data.txt",row.name=FALSE)