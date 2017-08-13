###
# run_analysis.R
# Source code for the Getting and Cleaning Data course project
# Author: Brady Tello
###

DATA_DIR = "./samsung/"
TEST_MARKER = 0
TRAIN_MARKER = 1

if(!dir.exists(DATA_DIR)){
  stop("the Samsung data must be in the current working directory.  Exiting.")
}

library(reshape)
library(dplyr)

# @param test_train: "test" for test set, "train" for train set
build_dataset <- function(test_train){
  target_dir = paste(DATA_DIR, test_train,sep="")
  
  # Load the IDs
  subject_ids <- read.csv(paste(target_dir, "/subject_", test_train, ".txt",sep=""), header=FALSE)
  names(subject_ids) = "ID"
  
  # Load feature names
  feature_labels <- as.character(read.table(paste(DATA_DIR,"features.txt",sep=""))[[2]])
  
  # Load data vectors
  dataset <- read.table(paste(target_dir,"/X_", test_train, ".txt",sep=""), 
                           strip.white = TRUE, 
                           header = FALSE, 
                           col.names = feature_labels)
  
  # select fields that contain mention of mean or stdev
  dataset <- select(dataset, grep("mean\\.|std\\.", names(dataset)))
  
  # Rename vars
  split_names <- strsplit(names(dataset), "\\.")
  combineNames <- function(x){ 
    if(length(x) >= 5){
      paste(x[1], x[2], x[5], sep="_") 
    } else { paste(x[1], x[2], sep="_")}
  }
  names(dataset) = sapply(split_names, combineNames)
  
  activity_labels <- read.table(paste(DATA_DIR,"activity_labels.txt",sep="")) %>% 
    rename(activity_id = V1, activityLabels = V2)
  
  train_labels <- read.table(paste(target_dir,"/y_",test_train, ".txt",sep=""), 
                             header = FALSE, 
                             col.names = c("activity_id")) %>%
                  merge(activity_labels, by="activity_id") %>%
                  select(activityLabels)
  
  # append labels and ids to the data
  dataset <- cbind(subject_ids, dataset, train_labels)
  
  # sanity check
  if(test_train == "train" && dim(dataset)[1] != 7352)
    { print("ERROR: The number of rows is not 7352...it should be")}
  
  if(test_train == "test" && dim(dataset)[1] != 2947)
    { print("ERROR: The number of rows is not 2947...it should be")}
  
  return(dataset)
}

###################
# Script entry point
###################

train_data <- build_dataset("train")
test_data <- build_dataset("test")

all_data <- rbind(train_data, test_data)

# sanity check
if(dim(all_data)[1] != (dim(test_data)[1] + dim(train_data)[1])){
  print("ERROR: Missing observations after appending test and training data")
}

# clean up
rm(train_data, test_data)

# create new datasets reporting on mean of each variable for each activity and subject
melted = melt(all_data, c("ID", "activityLabels"))
agg_means = cast(melted, ID + activityLabels~variable, mean)
new_names = paste("mean", names(agg_means)[-c(1,2)], sep="_")
names(agg_means) = append(c("ID", "activityLabels"), new_names)

print("tidy data resides in the variable agg_means")