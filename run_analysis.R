# -------------------------------------------------------------------------------------------------------
#
# Coursera: Data Science Course - 3 Getting and Cleaning Data Course Project
# @author   Amir Hamzah Khalid
# @since    2016-11-10
#
# run_analysis.R File Description:
#
# This script will perform the following steps on the UCI HAR Dataset downloaded from: 
# "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
#
# 1. Merge the training and the test sets to create one data set.
# 2. Extract only the measurements on the mean and standard deviation for each measurement. 
# 3. Use descriptive activity names to name the activities in the data set.
# 4. Appropriately label the data set with descriptive activity names. 
# 5. Creates a second, independent tidy data set with the average of each variable for each 
#    activity and each subject. 
#
# -------------------------------------------------------------------------------------------------------

# Clean up workspace
rm(list = ls(all = TRUE))

library(plyr)       # load plyr first, then dplyr 
library(data.table) # a prockage that handles dataframe better
library(dplyr)      # for fancy data table manipulations and organization

# Set working directory to the location for the project
    setwd('/Users/amirkhalid/Documents/Study/Data Science Specialization Course/3 Getting and Cleaning Data/Week 4/Peer Graded Assignment');

# Create Data folder in working directory if not exist
    if(!file.exists("./data")) { dir.create("./data") }

# Download zipfile into data folder
    fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
    download.file(fileUrl, destfile = "./data/Dataset.zip")

# Unzip dataSet to /data folder
    unzip(zipfile = "./data/Dataset.zip", exdir = "./data")

# Reading all data from files
    
    features      <- read.table('./data/UCI HAR Dataset/features.txt',colClasses = c("character"))                    #read features.txt
    activityLabel <- read.table('./data/UCI HAR Dataset/activity_labels.txt',col.names = c("ActivityId", "Activity")) #read activity_labels.txt
    subjectTrain  <- read.table('./data/UCI HAR Dataset/train/subject_train.txt')                                     #read subject_train.txt
    xTrain        <- read.table('./data/UCI HAR Dataset/train/x_train.txt')                                           #read x_train.txt
    yTrain        <- read.table('./data/UCI HAR Dataset/train/y_train.txt')                                           #read y_train.txt
    subjectTest   <- read.table('./data/UCI HAR Dataset/test/subject_test.txt')                                       #read subject_test.txt
    xTest         <- read.table('./data/UCI HAR Dataset/test/x_test.txt')                                             #read x_test.txt
    yTest         <- read.table('./data/UCI HAR Dataset/test/y_test.txt')                                             #read y_test.txt


# 1. Merge the 'training' and the 'test' sets to create one data set.
  
    # 1.1 Binding data
    trainingData <- cbind(cbind(xTrain, subjectTrain), yTrain)
    testData     <- cbind(cbind(xTest, subjectTest), yTest)
    finalData    <- rbind(trainingData, testData)
    
    # 1.2 Label columns
    finalDataLabels  <- rbind(rbind(features, c(562, "Subject")), c(563, "ActivityId"))[,2]
    names(finalData) <- finalDataLabels

    
# 2. Extract only the measurements on the mean and standard deviation for each measurement.

    finalData_mean_std <- finalData[,grepl("mean|std|Subject|ActivityId", names(finalData))]


# 3. Use descriptive activity names to name the activities in the data set

    finalData_mean_std <- join(finalData_mean_std, activityLabel, by = "ActivityId", match = "first")
    finalData_mean_std <- finalData_mean_std[,-1]

    
# 4. Appropriately label the data set with descriptive activity names.

    # 4.1 Remove parentheses
    names(finalData_mean_std) <- gsub('\\(|\\)',"",names(finalData_mean_std), perl = TRUE)
    
    # 4.2 Make syntactically valid names
    names(finalData_mean_std) <- make.names(names(finalData_mean_std))
    
    # 4.3 Change columns names (clearer names)
    names(finalData_mean_std) <- gsub('Acc',"Acceleration",names(finalData_mean_std))
    names(finalData_mean_std) <- gsub('GyroJerk',"AngularAcceleration",names(finalData_mean_std))
    names(finalData_mean_std) <- gsub('Gyro',"AngularSpeed",names(finalData_mean_std))
    names(finalData_mean_std) <- gsub('Mag',"Magnitude",names(finalData_mean_std))
    names(finalData_mean_std) <- gsub('^t',"TimeDomain.",names(finalData_mean_std))
    names(finalData_mean_std) <- gsub('^f',"FrequencyDomain.",names(finalData_mean_std))
    names(finalData_mean_std) <- gsub('\\.mean',".Mean",names(finalData_mean_std))
    names(finalData_mean_std) <- gsub('\\.std',".StandardDeviation",names(finalData_mean_std))
    names(finalData_mean_std) <- gsub('Freq\\.',"Frequency.",names(finalData_mean_std))
    names(finalData_mean_std) <- gsub('Freq$',"Frequency",names(finalData_mean_std))
    
    # Change subject names
    finalData_mean_std$Subject <- as.character(finalData_mean_std$Subject)
    finalData_mean_std$Subject[finalData_mean_std$Subject == 1] <- "Participant 1"
    finalData_mean_std$Subject[finalData_mean_std$Subject == 2] <- "Participant 2"
    finalData_mean_std$Subject[finalData_mean_std$Subject == 3] <- "Participant 3"
    finalData_mean_std$Subject[finalData_mean_std$Subject == 4] <- "Participant 4"
    finalData_mean_std$Subject[finalData_mean_std$Subject == 5] <- "Participant 5"
    finalData_mean_std$Subject[finalData_mean_std$Subject == 6] <- "Participant 6"
    finalData_mean_std$Subject[finalData_mean_std$Subject == 7] <- "Participant 7"
    finalData_mean_std$Subject[finalData_mean_std$Subject == 8] <- "Participant 8"
    finalData_mean_std$Subject[finalData_mean_std$Subject == 9] <- "Participant 9"
    finalData_mean_std$Subject[finalData_mean_std$Subject == 10] <- "Participant 10"
    finalData_mean_std$Subject[finalData_mean_std$Subject == 11] <- "Participant 11"
    finalData_mean_std$Subject[finalData_mean_std$Subject == 12] <- "Participant 12"
    finalData_mean_std$Subject[finalData_mean_std$Subject == 13] <- "Participant 13"
    finalData_mean_std$Subject[finalData_mean_std$Subject == 14] <- "Participant 14"
    finalData_mean_std$Subject[finalData_mean_std$Subject == 15] <- "Participant 15"
    finalData_mean_std$Subject[finalData_mean_std$Subject == 16] <- "Participant 16"
    finalData_mean_std$Subject[finalData_mean_std$Subject == 17] <- "Participant 17"
    finalData_mean_std$Subject[finalData_mean_std$Subject == 18] <- "Participant 18"
    finalData_mean_std$Subject[finalData_mean_std$Subject == 19] <- "Participant 19"
    finalData_mean_std$Subject[finalData_mean_std$Subject == 20] <- "Participant 20"
    finalData_mean_std$Subject[finalData_mean_std$Subject == 21] <- "Participant 21"
    finalData_mean_std$Subject[finalData_mean_std$Subject == 22] <- "Participant 22"
    finalData_mean_std$Subject[finalData_mean_std$Subject == 23] <- "Participant 23"
    finalData_mean_std$Subject[finalData_mean_std$Subject == 24] <- "Participant 24"
    finalData_mean_std$Subject[finalData_mean_std$Subject == 25] <- "Participant 25"
    finalData_mean_std$Subject[finalData_mean_std$Subject == 26] <- "Participant 26"
    finalData_mean_std$Subject[finalData_mean_std$Subject == 27] <- "Participant 27"
    finalData_mean_std$Subject[finalData_mean_std$Subject == 28] <- "Participant 28"
    finalData_mean_std$Subject[finalData_mean_std$Subject == 29] <- "Participant 29"
    finalData_mean_std$Subject[finalData_mean_std$Subject == 30] <- "Participant 30"
    finalData_mean_std$Subject <- as.factor(finalData_mean_std$Subject)
    

# 5. Create a second, independent tidy data set with the average of each variable for each activity and each subject.

    # 5.1 Create Master.dt
    avg_by_act_sub = ddply(finalData_mean_std, c("Subject","Activity"), numcolwise(mean))

    # 5.2 Write table into text and .csv file name 'Tidy'
    write.table(avg_by_act_sub, file = "Tidy.txt", row.names = FALSE)
    write.table(avg_by_act_sub, file = "Tidy.csv", row.names = FALSE, sep = ",")



