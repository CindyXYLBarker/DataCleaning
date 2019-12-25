prepareDataFile <- function(url, destFile){
  download.file(url, destFile)
  if (!file.exists(destFile)){
    stop(paste("File download failed ", url))
  }
  unzip(destFile)
}

main <- function(){
  zipFileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
  destFile <- "./data.zip"
  prepareDataFile(zipFileUrl, destFile)
}