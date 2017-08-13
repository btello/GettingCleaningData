if(!dir.exists("./samsung")){
  print("the Samsung data must be in the current working directory.  Exiting.")
  quit()
}

library(dplyr)

### Training set

# Load the training IDs
subject_train <- read.csv("./samsung/train/subject_train.txt", header=FALSE)
names(subject_train) = "ID"

# Load feature names for training vectors
feature_labels <- as.character(read.table("./samsung/features.txt")[[2]])

# Load training vectors
train_data <- read.table("./samsung/train/X_train.txt", 
                         strip.white = TRUE, 
                         header = FALSE, 
                         col.names = feature_labels)
train_labels <- read.table("./samsung/train/y_train.txt", header = FALSE, col.names = c("label"))

# TODO: Provide descriptions for labels in codebook

# column Combine the IDs with the training data
train_data <- cbind(subject_train, train_data, train_labels)

# mark as training data
train_data$testTrain = 1

# select fields that contain mention of mean or stdev...probably some sort of regex
train_data <- select(train_data, "ID", grep("mean|std", names(train_data)), "label")

# sanity check
if(dim(train_data)[1] != 7352){ print("ERROR: The number of rows is not 7352...it should be")}