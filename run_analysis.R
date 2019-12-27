library(dplyr)
library(stringr)

# Prepare data file, including download and unzip
prepareDataFile <- function(url, destFile){
  download.file(url, destFile)
  if (!file.exists(destFile)){
    stop(paste("File download failed ", url))
  }
  unzip(destFile)
}

# Load vector from txt file
loadVector <- function(dataFile){
  if (!file.exists(dataFile)){
    stop(paste("File does not exists ", dataFile))
  }  
  names <- read.table(dataFile)
  return(unlist(names[,2]))
}

# Load dataframe from txt file
loadDataFrame <- function(dataFile, names){
  if (!file.exists(dataFile)){
    stop(paste("File does not exists ", dataFile))
  }
  df <- read.table(dataFile)
  names(df) <- names
  return(df)
}

# Apply descriptive labels/names
prepareLabels <- function(names){
  newNames <- str_remove_all(names, "[\\(\\)]")
  newNames <- str_replace_all(newNames, "BodyBody", "Body")
  newNames <- paste("AVG-", newNames, sep="")
  newNames <- str_replace_all(newNames, "AVG-result", "Result")
  newNames <- str_replace_all(newNames, "AVG-subject", "Subject")
  return(newNames)
}

main <- function(){
  zipFileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
  destFile <- "./data.zip"
  dataFileDir <- "./UCI HAR Dataset/"
  prepareDataFile(zipFileUrl, destFile)
  
  # load all data frames with appropriate labels
  trainDf <- loadDataFrame(
    paste(dataFileDir, "train/X_train.txt", sep=""),
    loadVector(paste(dataFileDir, "features.txt", sep="")))
  trainLabelDf <- loadDataFrame(
    paste(dataFileDir, "train/y_train.txt", sep=""), c("result"))
  trainSubjectDf <- loadDataFrame(
    paste(dataFileDir, "train/subject_train.txt", sep=""), c("subject"))
  testDf <- loadDataFrame(
    paste(dataFileDir, "test/X_test.txt", sep=""),
    loadVector(paste(dataFileDir, "features.txt", sep="")))
  testLabelDf <- loadDataFrame(
    paste(dataFileDir, "test/y_test.txt", sep=""), c("result"))
  testSubjectDf <- loadDataFrame(
    paste(dataFileDir, "test/subject_test.txt", sep=""),c("subject"))
  
  # merge data frame
  mergeTrainDf <- cbind(trainDf, trainLabelDf, trainSubjectDf)
  mergeTestDf <- cbind(testDf, testLabelDf, testSubjectDf)
  df <- rbind(mergeTrainDf, mergeTestDf)
  
  # filter columes for mean and std only
  filteredCols <- grep("mean\\(\\)|std\\(\\)|result|subject", names(df), value=TRUE)
  filteredDf <- subset(df, select = filteredCols)
  
  # Add descriptive activity names
  activity <- loadVector(paste(dataFileDir, "activity_labels.txt", sep=""))
  filteredDf <- mutate(filteredDf, result=activity[result])
  
  # Generate tidy data set group by activity and subject
  tidyDf <- filteredDf %>% 
    group_by(result, subject) %>% 
    summarise_all(mean, na.rm=TRUE)
  
  # Clean up names
  names(tidyDf) <- prepareLabels((names(filteredDf)))
  
  return (tidyDf)
}