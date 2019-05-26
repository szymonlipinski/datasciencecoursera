# author: Szymon Lipiński

# Merges the training and the test sets to create one data set.
# Extracts only the measurements on the mean and standard deviation for each measurement.
# Uses descriptive activity names to name the activities in the data set
# Appropriately labels the data set with descriptive variable names.
# From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

# URL for the input data set 
data_url = "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"

# store the main path for later
main_path <- getwd()

# download the zip file with the data set
download.file(data_url, file.path(main_path, "dataFiles.zip"))

# unzip the downloaded file to the current directory
unzip(zipfile = "dataFiles.zip")

# Load activity_labels.txt
activityLabels <- fread(file.path(main_path, "UCI HAR Dataset/activity_labels.txt"), 
                        col.names = c("activityClass", "activityName"))

# Load the features.txt
features <- fread(file.path(main_path, "UCI HAR Dataset/features.txt"), 
                  col.names = c("featureLabel", "featureName"))

# We want to have only the mean and the standard deviation.
requiredFeatures <- grep("(mean|std)\\(\\)", features$featureName)

# The columns will be renamed a little bit
requiredFeaturesLabels <- gsub("[()]", "", features[requiredFeatures, ]$featureName)

measurments = features[requiredFeatures, ]

# load all the test files
testSet <- fread(file.path(main_path, "UCI HAR Dataset/test/X_test.txt"))[, requiredFeatures, with = FALSE]
testLabels <- fread(file.path(main_path, "UCI HAR Dataset/test/y_test.txt"), col.names = c("Activity"))
testSubjects <- fread(file.path(main_path, "UCI HAR Dataset/test/subject_test.txt"), col.names = c("SubjectNumber"))

# rename the columns so we use modified labels
names(testSet) <- requiredFeaturesLabels

# make a nice table with all the test columns joined
test <- cbind(testLabels, testSubjects, testSet)

# load all the train files
trainSet <- fread(file.path(main_path, "UCI HAR Dataset/train/X_train.txt"))[, requiredFeatures, with = FALSE]
trainLabels <- fread(file.path(main_path, "UCI HAR Dataset/train/y_train.txt"), col.names = c("Activity"))
trainSubjects <- fread(file.path(main_path, "UCI HAR Dataset/train/subject_train.txt"), col.names = c("SubjectNumber"))

# rename the columns so we use modified labels
names(trainSet) <- requiredFeaturesLabels

# make a nice table with all the traing columns joined
train <- cbind(trainLabels, trainSubjects, trainSet)

# combine the test and the train data sets
data <- rbind(test, train)

# translate the activity classes to activity names.
data$Activity <- factor(data[, Activity], levels=activityLabels$activityClass, labels=activityLabels$activityName)

# make the new data set with average stats for every activity and subject,
# and store it into a file
data %>%
    melt(id = c("Activity", "SubjectNumber")) %>%
    dcast(Activity + SubjectNumber ~ variable, fun = mean) %>%
    fwrite(file = file.path(main_path, "tidyData.csv"), quote = FALSE)


