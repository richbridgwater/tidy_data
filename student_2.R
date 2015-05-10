library(stringr)
library(plyr)
library(dplyr)

## Set the data file names that will be used below to be relative to the current working directory.  This
## assumes a folder structure with training and test data in separate child folders and everything else in 
## the main working directory
training_observations_file_name <- "UCI HAR Dataset/train/x_train.txt"
test_observations_file_name <- "UCI HAR Dataset/test/x_test.txt"
features_file_name <- "UCI HAR Dataset/features.txt"
training_subjects_file_name <- "UCI HAR Dataset/train/subject_train.txt"
test_subjects_file_name <- "UCI HAR Dataset/test/subject_test.txt"
activity_labels_file_name <- "UCI HAR Dataset/activity_labels.txt"
training_activities_file_name <- "UCI HAR Dataset/train/y_train.txt"
test_activities_file_name <- "UCI HAR Dataset/test/y_test.txt"
project_summarized_dataset_file_name <- "Getting-and-Cleaning-Data-Course-Project/har_summarized_dataset.txt"

## 1) Merge training and test datasets
## Read in feature names and convert them to valid column names
feature_column_names <- read.table(features_file_name)

## Read in training and test data
training_observations <- read.table(file = training_observations_file_name, col.names = feature_column_names[, 2])
test_observations <- read.table(file = test_observations_file_name, col.names = feature_column_names[, 2])

## Merge training and test observation datasets
har_all_observations_unfiltered <- rbind(training_observations, test_observations)


## 2) Filter out only the measurements for mean and standard deviation.  Here, any field with an 
##    occurrence of 'mean' or 'std' is kept
har_all_observations_filtered <- har_all_observations_unfiltered[, str_detect(string = colnames(har_all_observations_unfiltered), pattern = "(mean)|(std)")]


## 3) Read in activity data (test and train) at the ID level, add the activity labels, then bind it to the
##    merged data from the previous steps

activity_labels <- read.table(activity_labels_file_name, col.names = c("activityID", "activityName"))

## Read in activity datasets (train and test) and join them with their activity labels
training_activities <- read.table(training_activities_file_name, col.names = c("activityID"))
training_activities <- join(training_activities, activity_labels, by = "activityID")

test_activities <- read.table(test_activities_file_name, col.names = c("activityID"))
test_activities <- join(test_activities, activity_labels, by = "activityID")

## Merge the training and activity datasets together
all_activities <- rbind(training_activities, test_activities)

## Now merge the activity name column into the overall merged dataset
har_all_observations_filtered <- cbind(activityName = all_activities[, 2], har_all_observations_filtered)


## 3a) Reading in subject data (train and test) and adding it as a column to the previously merged
##     dataset.  
##     Note: Left out this data until now so it would be easier to filter out the measurement columns in the
##           previous steps.  Additionally, it was also easier to add this last since it was desireable to put
##           the subject data in the first column of the dataset to increase readability.
training_subjects <- read.table(training_subjects_file_name, col.names = c("subjectID"))
test_subjects <- read.table(test_subjects_file_name, col.names = c("subjectID"))
all_subjects <- rbind(training_subjects, test_subjects)

har_all_observations_filtered <- cbind(all_subjects, har_all_observations_filtered)


## 4) Label the variable names appropriately. They are already in pretty good shape, but performing 
##    some minor esthetics to make them more readable
new_column_names <- colnames(har_all_observations_filtered)

## Remove all occurrences of . and either replacing with a _ (it they occur in the middle of the name) or 
## removing entirely if they occur at the end of the name
new_column_names <- str_replace_all(new_column_names, "[\\.]+", "_")
new_column_names <- str_replace_all(new_column_names, "_$", "")
colnames(har_all_observations_filtered) <- new_column_names


## 5) From the dataset created above (all_observations_filter), create a new dataset with the mean for
##    each variable by subject by activity
har_varmeans_by_subject_activity <- ddply(har_all_observations_filtered, .(subjectID, activityName), numcolwise(mean))

## update the column names to reflect they contain the means calaculated above (i.e. just append _actmean
## to the relevant column names).  
## Note: Not touching the first two columns as they just identify the subject and activity
new_column_names <- colnames(har_varmeans_by_subject_activity)
new_column_names[-(1:2)] <- str_c(new_column_names[-(1:2)], "_actmean")
colnames(har_varmeans_by_subject_activity) <- new_column_names

## write the new dataset to a file
write.table(har_varmeans_by_subject_activity, project_summarized_dataset_file_name, row.names = FALSE)
