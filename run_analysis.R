###Author: Anya Mityushina (GitHub = AnyaMit)
### Date:  11/17/2015
### Project: Getting and Cleaning Data: run_Analysis.R skipt & tidy data


### Please make sure to view the ReadMe.md file and the CodeBook.md BEFORE running this script.


##Required packages - dowloand below
#install.packages("data.table")
#install.packages("reshape2")
#install.packages("plyr")
#install.packages("curl")


## Call the packages for use to R Studio
library(data.table)
library(reshape2)
library(plyr)
library(curl)


## Place run_analysis.R and UCI HAR Dataset in the working directory. To find out your directory - 
### Helpful User guidance to make sure code and data is in the same place 
      directory <- getwd()
      print("         Hello there. Before we begin, lets check that you have the proper files in")
      print("your working directory, which is here--->")
      print(directory) 



# NOTE: Below is two lines of code that will download the UCI HAR data for you via R.
# Please note that this code will populate the .zip file in your DOWNLOAD folder.
# You will have to navidate to your download folder and UnZip the files into your working directory - (identified above).
#---remove "##" from the next two lines of code to download the data. Ignore this if you've downloaded the data prior to this.

## file_source <-"https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
## download.file(url = "UCI HAR Dataset",destfile= file_source,method = "curl")


## Check to make sure all needed data and run_analysis are in your Working Directory.
      directory_files<-list.files(path = ".")
      print("...and this is what is in the working directory")
      print(directory_files)
      print("         You should see ---> run_analysis.R and UCI HAR Datase <----")

##Print a message and conduct analysis to combine data
      print("The code will now try to tidy up the data in all in the right ways")

###upload variables
      
print("Actions completed:")
print("     ")
print("Uploading variables")

features          <- read.table("C:/Users/Anya/Documents/GCD/UCI HAR Dataset/features.txt")
activities        <- read.table("C:/Users/Anya/Documents/GCD/UCI HAR Dataset/activity_labels.txt")
subject_test      <- read.table("C:/Users/Anya/Documents/GCD/UCI HAR Dataset/test/subject_test.txt")
subject_train     <- read.table("C:/Users/Anya/Documents/GCD/UCI HAR Dataset/train/subject_train.txt")
x_test            <- read.table("C:/Users/Anya/Documents/GCD/UCI HAR Dataset/test/X_test.txt")
y_test            <- read.table("C:/Users/Anya/Documents/GCD/UCI HAR Dataset/test/y_test.txt")
x_train           <- read.table("C:/Users/Anya/Documents/GCD/UCI HAR Dataset/train/x_train.txt")
y_train           <- read.table("C:/Users/Anya/Documents/GCD/UCI HAR Dataset/train/y_train.txt")

##combine x, y, subject to create one dataset.
x_data            <- rbind(x_train, x_test)
y_data            <- rbind(y_train, y_test)
sub_data          <-rbind(subject_train,subject_test)

colnames(x_data)  <- t(features[2])

print("combined datasets")

colnames(y_data)   <- "Activity"
colnames(sub_data) <- "Subject"
finalData          <- cbind(x_data,y_data,sub_data)

##Extracting only std and Mean, adding 2 rows for row names
subset_Data        <- grep(".*Mean.*|.*Std.*", names(finalData), ignore.case=TRUE)
needed_col         <- c(subset_Data,562,563)

subset_finalData   <- finalData[,needed_col]

print("subsetting worked")

###Create a loopfunction to look up Activity key for the data (1-6)

subset_finalData$Activity <-as.character(subset_finalData$Activity)
for (i in 1:6) {
  
  subset_finalData$Activity[subset_finalData$Activity == i] <- as.character(activities[i,2])
}


print("looping worked")


###Rename labels to be user friendly per instructions
names(subset_finalData)  <-gsub("Acc", "Accelerometer", names(subset_finalData))
names(subset_finalData)  <-gsub("Gyro", "Gyroscope", names(subset_finalData))
names(subset_finalData)  <-gsub("BodyBody", "Body", names(subset_finalData))
names(subset_finalData)  <-gsub("Mag", "Magnitude", names(subset_finalData))
names(subset_finalData)  <-gsub("^t", "Time", names(subset_finalData))
names(subset_finalData)  <-gsub("^f", "Frequency", names(subset_finalData))
names(subset_finalData)  <-gsub("tBody", "TimeBody", names(subset_finalData))
names(subset_finalData)  <-gsub("-mean()", "Mean", names(subset_finalData), ignore.case = TRUE)
names(subset_finalData)  <-gsub("-std()", "STD", names(subset_finalData), ignore.case = TRUE)
names(subset_finalData)  <-gsub("-freq()", "Frequency", names(subset_finalData), ignore.case = TRUE)
names(subset_finalData)  <-gsub("angle", "Angle", names(subset_finalData))
names(subset_finalData)  <-gsub("gravity", "Gravity", names(subset_finalData))

print("headers renamed")

subset_finalData$Subject <- as.factor(subset_finalData$Subject)
subset_finalData <- data.table(subset_finalData)


print("final subset done")

tidyData <- aggregate(. ~Subject + Activity, subset_finalData, mean)
tidyData <- tidyData[order(tidyData$Subject,tidyData$Activity),]
write.table(tidyData, file = "Tidy.txt", row.names = FALSE)

#
print("Tidy file should be now created")
print("")

averages_data <- ddply(tidyData, .(Subject, Activity), fun=function(x) {colMeans(x[, 1:66])} )

write.table(averages_data, "averages_data.txt", row.name=FALSE)

print("averages_data should now be created")



