PrepareSet1 <- function(rows = -1){
  library("data.table")
  
  ######Define static variable
  fileURL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
  zipFile <- "source.zip"
  source.directory <- "UCI HAR Dataset"

  src.datasets <- data.frame(features=c("train/X_train.txt", "test/X_test.txt"), 
                     activities=c("train/y_train.txt", "test/y_test.txt"),
                     subjects=c("train/subject_train.txt", "test/subject_test.txt"))
  
  tidy.data1.file <- "tidy_set1.csv"
  
  ###Verifying presence of source data and downloading if needed
  print("Verifying source data")
  
  if(!file.exists(source.directory)){
    if(!file.exists(zipFile)){
      print("Downloading source file")
      download.file(url=fileURL, destfile=zipFile, mode="wb")
    }else{
      print("Archived source file found")
    }
    print("Unarchiving source data")
    unzip(zipFile)
  }else{
    print("Source data found")
  }
  print("Source data ready")
  

  ###Read activities, subjects
  definitions <- read.definitions()
  
  data <- as.data.frame( setNames(replicate(length(c(definitions$features))+2, numeric(0), simplify = F), c("activity","subject", definitions$features)) )
  
  for(i in 1:nrow(src.datasets) ){
    tmp.data <- read.table(paste(source.directory, "/", src.datasets[i, 'features'], sep=""), 
                       colClasses = c(definitions$features_classes), 
                       #col.names = c(definitions$features),
                       comment.char = "", strip.white = TRUE, header=FALSE,
                       nrows = rows)
    
    tmp.subjects <- read.table(paste(source.directory, "/", src.datasets[i, 'subjects'], sep=""), 
                           colClasses = "numeric", 
                           col.names = c("subject"),
                           comment.char = "", strip.white = TRUE, header=FALSE,
                           nrows = rows)
    
    tmp.activities <- read.table(paste(source.directory, "/", src.datasets[i, 'activities'], sep=""), 
                               colClasses = "factor", 
                               col.names = c("subject"),
                               comment.char = "", strip.white = TRUE, header=FALSE,
                               nrows = rows)
    
    tmp.data <- cbind(tmp.activities, tmp.subjects, tmp.data)
    
    data <- rbind(data, tmp.data)
    
    #print(paste(i, ": ", nrow(tmp.data), ": ", nrow(data)))
  }
  
  colnames(data) <- c("activity", "subject", definitions$features)
  
  data <- data.table(data)

  setkey(data, activity)
  
  #Replace activity index "activity" with the activity name
  data <- merge(data, definitions$activities)
  data <- data[, "activity" := NULL]
  
  #Save the tidy data set into the file
  write.csv(data,tidy.data1.file, row.names=FALSE)
  
  #Return the tidy data set if it is needed for the calling entity
  invisible(data)
}


read.definitions <- function(srource.directory = "UCI HAR Dataset"){

  #  1. The function reads a list of selected features which will have to be included into the tidy data set
  #   The list is prepared manually based on the source data cook book.
  #   The list is stored in the definitions$features table
  #  2. The function populates activities factors into definitions$activities as data.table so that this can
  #   then be used for merging with the measurements data set
  definitions <- list()
  definitions$features   <- read.csv(paste("features_selected.csv", sep=""), strip.white = TRUE, colClasses = c("character", "NULL"))
  definitions$activities <- data.table(read.table(paste(srource.directory, "/", "activity_labels.txt", sep=""), colClasses = c("factor", "factor"), col.names=c("activity", "activity_name"), strip.white = TRUE))

  setkey(definitions$activities, activity)
  
  definitions$features_classes <- ifelse(definitions$features == "NULL", "NULL", "numeric")
  definitions$features <- definitions$features[definitions$features != "NULL"]
  
  definitions
}

PrepareSet1()
