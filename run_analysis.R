# This R script was created and tested on a MAC computer.
# The data folder needs to be located in your working directory and
# have the name "UCI HAR Dataset" for the script to work properly.

# Assignment objectives
You should create one R script called run_analysis.R that does the following. 
1. Merges the training and the test sets to create one data set.
2. Extracts only the measurements on the mean and standard deviation for each measurement. 
3. Uses descriptive activity names to name the activities in the data set
4. Appropriately labels the data set with descriptive variable names. 
5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

# Ok here we go...
# Let's first read in the features.txt file

      features <- read.table("UCI HAR Dataset/features.txt") 

# Now, I want to figure out columns to extract from the test and training data sets.
# We know we want the "mean" and "standard deviation" measurements, so I will 
# search the newly created "features" object and extract two subsets 
# based on the character strings "mean()" and "std()"

      # extract a subset of mean measurement labels from "features"
      subset_mean <- features[grep("mean()", features$V2, fixed = TRUE), ] 

      # extract a subset of standard deviation measurement labels from "features"     
      subset_std <- features[grep("std()", features$V2, fixed = TRUE), ]

      # then I will combine the mean and std measurement label subsets into one table
      subset_cmb <- rbind(subset_mean, subset_std)


# Let's import the remaining test and training data files

# Testing data --------------------------------------------------------------------

      # get test subject IDs - 1 variable 
      subject_test <- read.table("UCI HAR Dataset/test/subject_test.txt")

      # get test data measurements - 561 variables
      x_test <- read.table("UCI HAR Dataset/test/X_test.txt")

      # get test actyivity labels codes - 1 variable
      y_test <- read.table("UCI HAR Dataset/test/y_test.txt")


# Training data -------------------------------------------------------------------

      # get training subjects IDs - 1 variable
      subject_train <- read.table("UCI HAR Dataset/train/subject_train.txt")

      # get training data measurements - 561 variables
      x_train <- read.table("UCI HAR Dataset/train/X_train.txt")

      # get training label codes - 1 variable
      y_train <- read.table("UCI HAR Dataset/train/y_train.txt")

# Now I'll start putting together the final test and training dataframes.

# I'll use just the "subset_cmb" object and extract the mean and 
# standard deviation columns from the test and training dataframes that have 561 columns
# Which will reduce it to 66 columns.
      
      x_test <- select(x_test, subset_cmb$V1)
      x_train <- select(x_train, subset_cmb$V1)

# Let's now add the activity labels using cbind to both those new test and training tables.  
      
      test_bind <- cbind(y_test, x_test)
      train_bind <- cbind(y_train, x_train)

# Then we can add the subject IDs to both those tables, again using cbind. 

      test_bind2 <- cbind(subject_test, test_bind)
      train_bind2 <- cbind(subject_train, train_bind)

# I now have two tables for test and training structured 
# like this --> | subject | activity | ME1 | ME2 | ME3 | ME4 ...and so on.

# Now I can combine both those tables (test and training) into one.

      final_table <- rbind(test_bind2, train_bind2)

# Next I want to clean up the column names and remove the parenthesis and dashes from the final table

      new_var_names <- gsub("\\(\\)","", subset_cmb$V2)
      new_var_names <- gsub("\\-","", new_var_names)

# create vectors for first two column names
      
      subject_label <- "subject"
      activity_label <- "activity"

# combine all the column names
      
      final_column_labels <- c(subject_label,activity_label,new_var_names)

# rename the column names in the final table
      
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

# Convert to dplyr dataframe 
      
      final_table <- tbl_df(final_table)

# Finally, we'll group everything by the subject and activity columns and 
# apply the summarise_each function to calculate the mean 
      
      tidy_data <- final_table%>%group_by(subject,activity)%>%summarise_each(funs(mean))

# Let's take a look at the final output
print(tidy_data)


# sum(tested3[,1])
# [1] 11.07991
# 11.07991/50
# [1] 0.2215982