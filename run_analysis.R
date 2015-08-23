# edgardo veliz limas

## Here are the data for the project:
## https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip
## You should create one R script called run_analysis.R that does the following.
## 1. Merges the training and the test sets to create one data set.
## 2. Extracts only the measurements on the mean and standard deviation for each measurement.
## 3. Uses descriptive activity names to name the activities in the data set.
## 4. Appropriately labels the data set with descriptive variable names.
## 5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

# libraries
library(tools)
library(knitr)

SetWorkingDirectory <- function () {
  basePath <- "C:/Lab/RStudio/DSS-3_GCD"
  variablePath <- "/CP"
  path <- paste(basePath, variablePath, sep="")
  setwd(path)
}

GetParameters <- function() {
  # Return a list of parameters, all the info extrated before any processing of the data
  p <- list()

  p$FileURL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
  p$FileName <- "UCI HAR Dataset.zip"
  p$DirName <- "UCI HAR Dataset"
  p$ISDirName <- "Inertial Signals" # Inertial Signals directory name
  
  p$DataSets <- c("test", "train") # types of data sets
  p$Extension <- ".txt" # extension of the files
  
  # Names of the files (without extension) which contain the info indicated on the left  
  p$SubjectId <- "subject"
  p$ActivityId <- "y"
  p$ActivityLabel <- "activity_labels"
  p$CNFeatureVector <- "features" # column names of feature vector
  p$FeatureVector <- "X"
  p$InertialSignals <- c("total_acc_x", "total_acc_y", "total_acc_z", 
                         "body_acc_x", "body_acc_y", "body_acc_z", 
                         "body_gyro_x", "body_gyro_y", "body_gyro_z")

  # Other values
  p$nFeatureVector <- 561 # from de README.txt file (line 15)
  p$nSample <- 128 # from de README.txt file (lines 15 and 49)
  
  # p$nCol and p$Index are used to keep a record of the what columns form the data set
  # (SubjectId (x1), ActivityId (x1), ActivityLabel (x1), FeatureVector (x561), (x128xIS)
  p$nCol <- c(1, 1, 1, p$nFeatureVector, rep(p$nSample, length(p$InertialSignals)))
  
  value <- 1
  p$Index <- list()
  for (i in c(1:length(p$nCol))) {
    p$Index[[i]] <- c(value:(value + p$nCol[i] - 1))
    value <- value + p$nCol[i]
  }
  
  
  return(p)
}

DownloadData <- function (p) {
  if (!file.exists(p$FileName))
    download.file(p$FileURL, destfile=p$FileName, method="curl")
  if (!file.exists(p$DirName))
    unzip(p$FileName)
}

GetInfo <- function(p) {
  # Returns a list that contains:
  #   cn: character list that represent the column names of the data.
  #   dfActivityTable: data frame with the Id and Label for each activity.
  #   
  # cn
  cn <- list()
  # Subject.Id instead of "Id" because of how do.call(cbind, list) works with
  # data frames with only one column: in that case, a prefix with the name of the
  # object is no added.
  cn$Subject <- c("Subject.Id")
  cn$Activity <- c("Id", "Label")
  fnCNFeatureVector <- paste0(p$DirName, "/", p$CNFeatureVector, p$Extension)
  cn$FeatureVector <- read.table(fnCNFeatureVector, colClasses=c("NULL", "character"))[, 1]
  
  sInertialSignals <- c("TotalAcc", "BodyAcc", "BodyGyro")
  sAxis <- c("X", "Y", "Z")
  sSamples <- c(1:p$nSample)
  sInertialSignalsByAxis <- as.vector(t(outer(sInertialSignals, sAxis, paste, sep="-")))
  cn$InertialSignals <- t(outer(sInertialSignalsByAxis, sSamples, paste, sep=",")) 
  # cn$InertialSignals is equal to cartesian product of sInertialSignals, sAxis and sSamples
  colnames(cn$InertialSignals) <- sInertialSignalsByAxis 
  # colnames() is used instead of names() because the object is a matrix.
  
  # dfActivityTable
  fnActivityTable <- paste0(p$DirName, "/", p$ActivityLabel, p$Extension)
  dfActivityTable <- read.table(file=fnActivityTable, colClasses=c("factor", "character"))
  names(dfActivityTable) <- cn$Activity
  info <- list(cn=cn, dfActivityTable=dfActivityTable)
  return(info)
}

GetFileNames <- function (ds, p) {
  # Return a list of the files names corresponding to a data set
  fn <- list()
  fn$Subject <- paste0(p$DirName, "/", ds, "/", p$SubjectId, "_", ds, p$Extension)
  fn$Activity <- paste0(p$DirName, "/", ds, "/", p$ActivityId, "_", ds, p$Extension)
  fn$FeatureVector <- paste0(p$DirName, "/", ds, "/", p$FeatureVector, "_", ds, p$Extension)
  fn$InertialSignals <- paste0(p$DirName, "/", ds, "/", p$ISDirName, "/", p$InertialSignals, "_", ds, p$Extension) 
  return(fn)  
}

ReadDataSet <- function (ds, p, info) {
  fn <- GetFileNames(ds, p)
  df <- list()
  
  # subject
  df$Subject <- read.table(file=fn$Subject, colClass=c("factor"))
  levels(df$Subject[, 1]) <- sort(as.integer(levels(df$Subject[, 1])))
  names(df$Subject) <- info$cn$Subject
  
  # activity
  id <-  read.table(file=fn$Activity, colClasses=c("factor"))[, 1]
  levels(id) <- sort(as.integer(levels(id)))
  label <- info$dfActivityTable[match(id, info$dfActivityTable[, 1]), 2]
  df$Activity <- data.frame(id, label)
  names(df$Activity) <- names(info$dfActivityTable)
  
  # feature vector
  df$FeatureVector <- read.table(file=fn$FeatureVector, colClasses=rep("numeric", p$nFeatureVector))
  names(df$FeatureVector) <- info$cn$FeatureVector
  
  # inertial signals
  df$InertialSignals <- do.call(cbind, lapply(fn$InertialSignals, read.table, colClasses=rep("numeric", p$nSample)))
  names(df$InertialSignals) <- as.vector(info$cn$InertialSignals)
  
  # all
  dfDataSet <- do.call(cbind, df)
  return(dfDataSet)
}

ReadDataSets <- function(p, info) {
  dfDataSets <- lapply(p$DataSets, ReadDataSet, p, info)
  names(dfDataSets) <- p$DataSets
  return(dfDataSets)
}

MergeDataSets <- function(dfDataSets) {
  # pre-condition: the data sets have the same structure
  data <- do.call(rbind, dfDataSets)
  levels(data$Subject.Id) <- sort(as.integer(levels(data$Subject.Id)))
  levels(data$Activity.Id) <- sort(as.integer(levels(data$Activity.Id)))
  data <- data[order(data$Subject.Id, data$Activity.Id), ]
  row.names(data) <- NULL
  return(data)
}

WriteDataSet <- function(p, ds, dfDataSet) {
  # p: parameters
  # ds: name of the data set (e.g. train, test, all)
  # dfDataSet: data frame which contains the data to be saved
  
  if (!file.exists(paste0(p$DirName, "/", ds))) {
    dir.create(paste0(p$DirName, "/", ds))
  }
  if (!file.exists(paste0(p$DirName, "/", ds, "/", p$ISDirName))) {
    dir.create(paste0(p$DirName, "/", ds, "/", p$ISDirName))
  }
  
  fn <- as.character(unlist(GetFileNames(ds, p))) # getting file names

  index <- p$Index[-3] # removing activity label
  dfDataSets <- lapply(X=index, FUN=function(i, data) return(subset(x=data, select=i)), data=dfDataSet)
  dfDataSets[[1]][, 1] <- as.integer(dfDataSets[[1]][, 1])
  dfDataSets[[2]][, 1] <- as.integer(dfDataSets[[1]][, 1])
  invisible(mapply(FUN=write.table, dfDataSets, fn, row.names=F, col.names=F))
}

SetWorkingDirectory()
# 0.a. Defining the parameters 
p <- GetParameters()

# 0.b. Download the data sets
DownloadData(p)

# 0.c. Preprocessing the data and getting info from it  
info <- GetInfo(p)

# 1. Merges the training and the test sets to create one data set.
#    In that respect, according to https://class.coursera.org/getdata-031/forum/thread?thread_id=28, it
#    is not necessary to include the "Inertial Signals" directory, albeit, a complete merge of the data
#    must be include it; therefore, the next merge process accomplishs that requirement. 
dfDataSets <- ReadDataSets(p, info)
data <- MergeDataSets(dfDataSets)
rm(dfDataSets)
WriteDataSet(p, ds="all", data) # it is not required to save the merged data, but it has done

# 2. Extracts only the measurements on the mean and standard deviation for each measurement
data <- data[, c(1:3, grep("-mean\\(\\)|-std\\(\\)", colnames(data)))] # 1:3 includes Subject, ActivityId and ActivityLabel

# 3. Uses descriptive activity names to name the activities in the data set
# Already done in part 1, specifically in the reading of the data sets

# 4. Appropriately labels the data set with descriptive variable names.
temp <- colnames(data)
prefixNames <- c(".t", ".f", "Acc", "Gyro", "Mag", "BodyBody")
properNames <- c(".Time", ".Frequency", "Accelerometer", "Gyroscope", "Magnitude", "Body")

for (i in c(1:length(prefixNames))) {
  temp <- gsub(pattern=prefixNames[i], replacement=properNames[i], x=temp, fixed=TRUE)
}
colnames(data) <- temp

# 5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
tidyData <- aggregate(. ~Subject.Id + Activity.Label, data, mean)
tidyData <- tidyData[order(tidyData$Subject.Id, tidyData$Activity.Id), ]
rownames(tidyData) <- NULL
write.table(tidyData, file = "tidyData.txt")

#knit2html("codebook.Rmd")
