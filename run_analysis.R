##############################################################################
# I have written a function to read the data from a file, 
# as well as a function to open all the relevant files within
# a specific folder (test/train).
# Each step in the assignment has been laid out below, completed
# one by one.
# The final 'tidy' data set is saved to a .csv file.
##############################################################################




# Download file if it doesn't exist
if (!file.exists("Course Project")) {
        dir.create("Course Project")
}
fileZipName <- "./Course Project/UCIHARDataset.zip"
if(!file.exists(fileZipName)) {
        fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
        download.file(fileUrl,fileZipName,method="curl")
        dateDownloaded <- date()
}


# Unzip file if it doesn't exist
dirName <- "./Course Project/UCI HAR Dataset" 
if(!file.exists(dirName)) {
        dir.create(dirName)
        unzip(fileZipName,)
}


# Read data into a data frame
readfile <- function(dirName,fileName,colNames=NULL) {
        file <- paste(dirName,fileName,sep="/")
        if(is.null(colNames)) {
                data<-read.table(file,sep="")
        } else {
                data<-read.table(file,sep="",col.names=colNames)
        }
        return(data)
}

# Read all the files in the (test/train) datasets - uses the readfile function
readdata <- function(dataset,...) {
        dir<-paste(dirName,dataset,sep="/")
        X <- readfile(dir,paste("X_",dataset,".txt",sep=""),colNames=features$V2)
        Y <- readfile(dir,paste("Y_",dataset,".txt",sep=""),colNames="activity")
        subject <- readfile(dir,paste("subject_",dataset,".txt",sep=""),colNames="subject_id")
        return(cbind(subject,Y,X))
}



# Read features and all data from both test and train folders
features <- readfile(dirName,fileName="features.txt")
test <- readdata("test",dirName=dirName)
train <- readdata("train",dirName=dirName)



# Now all the data has been loaded into R, the assignment steps can be completed:

#Steps:######################################################################################
#1 - Merges the training and the test sets to create one data set.                          #
#2 - Extracts only the measurements on the mean and standard deviation for each measurement.#
#3 - Uses descriptive activity names to name the activities in the data set                 #
#4 - Appropriately labels the data set with descriptive variable names.                     #
#5 - From the data set in step 4, creates a second, independent tidy                        #
#    data set with the average of each variable for each activity and each subject.         #
#############################################################################################


# 1:
DF <- rbind(train,test)

# 2:
columns <- colnames(DF)
mean_std <- DF[,c(1,2,grep("mean",columns),grep("std",columns))]

# 3:
activityNames <- readfile(dirName,"activity_labels.txt",colNames=c("level","label"))

# 4:
mean_std$activity <- factor(mean_std$activity,levels=activityNames$level,labels=activityNames$label)

# 5:
library(plyr)
tidy<- ddply(mean_std,.(subject_id,activity),function(x){colMeans(x[,-c(1:2)])})
write.table(mean_std,paste(dirName,"mean_std.txt",sep="/"),row.name=FALSE)
write.table(tidy,paste(dirName,"tidy dataset.txt",sep="/"),row.name=FALSE)