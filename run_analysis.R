library(data.table)
library(reshape2)

# Load activity labels and features
activityLabels <- fread(file.path(getwd(), "UCI HAR Dataset/activity_labels.txt")
                        , col.names = c("classLabels", "activityName"))
features <- fread(file.path(getwd(), "UCI HAR Dataset/features.txt")
                  , col.names = c("index", "featureNames"))
featuresWanted <- grep("(mean|std)\\(\\)", features[, featureNames])
measurements <- features[featuresWanted, featureNames]
measurements <- gsub('[()]', '', measurements)

# This function loads a specified data set and saves it into a variable
load.data <- function(data.set) {
    path = paste(getwd(), "/raw_data/UCI HAR Dataset/", data.set)
    data <- fread(file.path(path, "/X_", data.set, ".txt"))[, featuresWanted,
                                                            with = FALSE]
    data.table::setnames(data, colnames(data), measurements)
    activities <- fread(file.path(path, "/Y_", data.set, ".txt")
                        , col.names = c("Activity"))
    subjects <- fread(file.path(path, "/subject_", data.set, ".txt")
                      , col.names = c("SubjectNum"))
    data <- cbind(subjects, activities, data)
    return(data)
}

# Load data sets
test.data <- load.data("test")
train.data <- load.data("train")

# Merge data sets
full.data <- rbind(train.data, test.data)

# Convert classLabels to activityName. 
full.data[["Activity"]] <- factor(full.data[, Activity]
                                  , levels = activityLabels[["classLabels"]]
                                  , labels = activityLabels[["activityName"]])

full.data[["SubjectNum"]] <- as.factor(full.data[, SubjectNum])
full.data <- reshape2::melt(data = full.data, id = c("SubjectNum", "Activity"))
full.data <- reshape2::dcast(data = full.data, SubjectNum + Activity ~ variable
                             , fun.aggregate = mean)

data.table::fwrite(x = full.data, file = "tidyData.txt", quote = FALSE)