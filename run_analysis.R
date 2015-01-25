1 WALKING
2 WALKING_UPSTAIRS
3 WALKING_DOWNSTAIRS
4 SITTING
5 STANDING
6 LAYING

summarise(by_subject_activity, mean($col1))

Option A.
subject |  activity |  sd | mean

1 "WALKING" -0.319105810200331 0.452722654264638
2 "WALKING" -0.385708093716054 0.471751410540144

filter() (and slice())
arrange()
select() (and rename())
distinct()
mutate() (and transmute())
summarise()
sample_n() and sample_frac()

- 'features_info.txt': Shows information about the variables used on the feature vector.
- 'features.txt': List of all features.
- 'activity_labels.txt': Links the class labels with their activity name.
- 'train/X_train.txt': Training set.
- 'train/y_train.txt': Training labels.
- 'test/X_test.txt': Test set.
- 'test/y_test.txt': Test labels.

(if activity column == 1 = "WALKING" and so on),

You should create one R script called run_analysis.R that does the following. 
1. Merges the training and the test sets to create one data set.
2. Extracts only the measurements on the mean and standard deviation for each measurement. 
3. Uses descriptive activity names to name the activities in the data set
4. Appropriately labels the data set with descriptive variable names. 
5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

# Import all variable labels for test and train data sets

features <- read.table("UCI HAR Dataset/features.txt") # for MAC

# Subset of mean measurements
subset_mean <- features[grep("mean()", features$V2, fixed = TRUE), ] # fixed = TRUE will do an exact match / FALSE = character match

# Subset of standard deviation measurements
subset_std <- features[grep("std()", features$V2, fixed = TRUE), ]


#goal is to create one table -> |subject|session|activity|ME1|ME2|ME3|ME4|
# use select() to combine into one table
# use two columns to extract data from X_test
# select total$V1 from X_test
# Give new labels to the measurment variables

# Import test data ---------------------------------------------------------------------------------------

# test subjects 2-24
subject_test <- read.table("UCI HAR Dataset/test/subject_test.txt")
# test results - 561 variables
x_test <- read.table("UCI HAR Dataset/test/X_test.txt")
# test actyivity labels - 6 variables
y_test <- read.table("UCI HAR Dataset/test/y_test.txt")


#Import train data ------------------------------------------------------------------------------------------

# subjects 1-30
subject_train <- read.table("UCI HAR Dataset/train/subject_train.txt")
# training set - 561 variables
x_train <- read.table("UCI HAR Dataset/train/X_train.txt")
# training labels
y_train <- read.table("UCI HAR Dataset/train/y_train.txt")


# Combine mean and std features 
subset_cmb <- rbind(subset_mean, subset_std)

# convert TESTING data df to dplyr df - makes a bit easier
x_test <- tbl_df(x_test)
# convert TRAINING data df to dplyr df
x_train <- tbl_df(x_train)

# convert activity label lists to dplyr df 
y_test <- tbl_df(y_test)
y_train <- table_df(y_train)

# Now we need to select only the subset cmb variables from the test dataset (x_test)
tester <- select(x_test, subset_cmb$V1)
# now we do the same for the training dataset (x_train)
trainer <- select(x_train, subset_cmb$V1)

# now we need to bind the activity lables to filtered test subset data
test_bind <- cbind(y_test, tester)
train_bind <- cbind(y_train, trainer)

# now we need to bind subjects to the those two tables

test_bind2 <- cbind(subject_test, test_bind)
train_bind2 <- cbind(subject_train, train_bind)

# Now the measurement variables only include the mean and std values, 
# and the the subjects for test and train are attached to those new dataframes
final_table <- rbind(test_bind2, train_bind2)

#cleanup features for new variable names

new_var_names <- gsub("\\(\\)","", subset_cmb$V2)
new_var_names <- gsub("\\-","", new_var_names)

# create vectors for subject and activity column names
subject_label <- "subject"
activity_label <- "activity"

# combine vectors
final_column_labels <- c(subject_label,activity_label,new_var_names)
# rename variables in final table
colnames(final_table) <- final_column_labels

#convert activity table to character to avoid cohersion errors
final_table$activity <- as.character(final_table$activity)
# convert all activity code numbers to text labels
final_table$activity[final_table$activity %in% "1"] <- "walking"
final_table$activity[final_table$activity %in% "2"] <- "walking_up"
final_table$activity[final_table$activity %in% "3"] <- "walking_dn"
final_table$activity[final_table$activity %in% "4"] <- "sitting"
final_table$activity[final_table$activity %in% "5"] <- "standing"
final_table$activity[final_table$activity %in% "6"] <- "laying"
summer <- final_table
summer <- tbl_df(final_table)
#Apply one or more functions to one or more columns. 
#Grouping variables are always excluded from modification.
summer%>%group_by(subject,activity)%>%summarise_each(funs(mean))
df %>% group_by(grp) %>% summarise_each(funs(mean))

> sum(tested3[,1])
[1] 11.07991
> 11.07991/50
[1] 0.2215982