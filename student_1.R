# Developed as a projec for Coursera Getting and Cleaning Data
# TD, December 17, 2014

library(plyr)
library(tidyr)

########################################################################################
# Part 1. Merge the training and the test sets to create one data set
########################################################################################

# see if the data is in the local working directory 
if (!file.exists("UCI HAR Dataset") 
    | !file.exists("UCI HAR Dataset/test/X_test.txt")
    | !file.exists("UCI HAR Dataset/test/subject_test.txt")
    | !file.exists("UCI HAR Dataset/test/y_test.txt")
    | !file.exists("UCI HAR Dataset/train/X_train.txt")
    | !file.exists("UCI HAR Dataset/train/subject_train.txt")
    | !file.exists("UCI HAR Dataset/train/y_train.txt")  )
  
{
  message("Data not found in the working directory. Downloading data...")
  temp <- tempfile()
  download.file("https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip",
                temp,method="curl")
  unzip(temp)
  unlink(temp)
  
}

# Read the training data and put it in one data frame

message("Reading the training data...")

train.set <-read.table("UCI HAR Dataset/train/X_train.txt")
train.subject <- read.table("UCI HAR Dataset/train/subject_train.txt")
train.activity <- read.table("UCI HAR Dataset/train/y_train.txt")

train.data <- cbind(train.subject,train.activity,train.set)


# Read the testing data and put it in one data frame

message("Reading the testing data...")

test.set <-read.table("UCI HAR Dataset/test/X_test.txt")
test.subject <- read.table("UCI HAR Dataset/test/subject_test.txt")
test.activity <- read.table("UCI HAR Dataset/test/y_test.txt")

test.data <- cbind(test.subject,test.activity,test.set)


# Read the features

message("Reading the features file...")

features <- read.table("UCI HAR Dataset/features.txt")


# make the colnames - note that subjectID and activity do not appear in the features.yxt
# and should therefore be added

new.headings <- c("subjectID","activity")
col.names <- append(new.headings,as.character(features[,2]))

# make the data frame with the data and assign it the col names derived from 
# features.txt

data <- rbind(train.data,test.data)
names(data) <- col.names

########################################################################################
# Part 2. Extract only the measurements on the mean and standard deviation for each 
#         measurement
########################################################################################

message("Selecting the required data...")

# we only need the columns 1, 2 and the ones that contain std() or mean() in their name
cols.to.select <-grep("mean\\(|std\\(",col.names)
selected.data <- data[,c(1,2,cols.to.select)]

# remove the clutter
message("Data successfully selected. Removing big chunks of data that is not needed anymore.")

rm(data)
rm(train.set)
rm(test.set)

########################################################################################
# Part 3. Use descriptive activity names to name the activities in the data set
########################################################################################

# make the activities in selected.data descriptive

message("Making the activity names descriptive...")

activity.labels <- read.table("UCI HAR Dataset/activity_labels.txt")

# actvity.labels contains two columns -  first is the activity number (same as the activity
# column in selected.data) second is the descriptive name, such as WALKING, SITTING, etc

make.decriptive <- function(x) 
{
  x <- activity.labels[x,2] #get the data from the second column
  # get rid of the underscores in the activity description (likely not necessary)
  x <-  gsub("_"," ",x)
}


selected.data$activity <- sapply(selected.data$activity,make.decriptive)
selected.data$activity <- as.factor(selected.data$activity)

# I tried to accomplishthe above by using merge or join, but that created an extra column 
# that I would need to delete. Note that I did change names in activity.labels to contain
# the label "activity"
# please tell me if you know how to accomplish the above using a single instruction
# merge(selected.data,activity.labels,by_x="activity",by_y="activity")
# join(selected.data, activity.labels, by = "activity")


########################################################################################
# Part 4. Appropriately label the data set with descriptive variable names.
########################################################################################

# It is not completely clear to me what should be done here, as the requirements state 
# that column names should be
#                       1. All lower case when possible
#                       2. Descriptive (Diagnosis versus Dx)
#                       3. Not duplicated
#                       4. Not have underscores or dots or white spaces 
# If I was to apply the above rules to the variable names, I would get names that look
# like freqbodyaccjerkxmean so I decided to make the variable names more readable (as in 
# interpreting point 1 as not possible) using camelCase. The above var name becomes
# freqBodyAccJerkXMean. I still don't like the names, but they are a bit better.

message("Tidying the headings...")

tidy.heading <- function(x) {
  # remove duplication of the word Body from some headings
  x <- sub("BodyBody","Body",x)
  # replace the trailing t with the word time
  x <- sub("^t","time",x)
  # replace the trailing f with the abbriviation freq
  x <- sub("^f","freq",x)
  # swap the words mean or std and the direction (X, Y, Z)
  # for example "freqBodyGyro-mean()-X" becomes "freqBodyGyroXmean"
  x <- sub("\\-(.+)\\(\\)\\-(.)","\\2\\1",x)
  # remove '-' before std or mean in names that do not contain direction
  # for example "freqBodyAccJerkMag-std()" becomes "freqBodyAccJerkMagstd"
  x <- sub("\\-(.+)\\(\\)", "\\1",x)
  # capitalize the word mean
  x <- sub("mean","Mean",x)
  #capitalize the word std - seems like the right thing to do
  x <- sub("std","STD",x)
}

# tidy the names of selected.data 
names(selected.data)<-sapply(names(selected.data),tidy.heading)

########################################################################################
# Part 5. From the data set in step 4, create a second, independent tidy data set 
#         with the average of each variable for each activity and each subject
########################################################################################

message("Tidying the data...")
# summarize the data first as requested
summary.data <- ddply(selected.data,.(subjectID,activity), colwise(mean))

# tidy the data
tidy.data <- gather(summary.data,"measurement"," meanValue",timeBodyAccXMean:freqBodyGyroJerkMagSTD)

tidy.data$activity <- as.factor(tidy.data$activity)
tidy.data$measurement <- as.factor(tidy.data$measurement)

# sort the data - the assignment doesn't require this step, but if someone presented data to
# me, I would like to see std close to mean of the measurement

tidy.data <- arrange(tidy.data,subjectID, activity,measurement)

# write the summarized data into a file
message("Writing the tidy data in the file tidy_data.txt in your UCI HAR Dataset folder")
write.table(tidy.data,file = "UCI HAR Dataset/tidy_data.txt", sep="\t", row.name = FALSE)

# print the tidy.data on the screen

print(tidy.data)