#Load required packages
library(data.table)
library(reshape2)
#Set working directory
path <- getwd()
#Load data
Input <- file.path(path, "UCI HAR Dataset")
Subject_Train <- read.table(file.path(Input, "train", "subject_train.txt"))
Subject_Test  <- read.table(file.path(Input, "test" , "subject_test.txt" ))
Activity_Train <- read.table(file.path(Input, "train", "Y_train.txt"))
Activity_Test  <- read.table(file.path(Input, "test" , "Y_test.txt" ))
Train <- read.table(file.path(Input, "train", "X_train.txt"))
Test <- read.table(file.path(Input, "test", "X_test.txt"))
#Merge data sets
Subject <- rbind(Subject_Train, Subject_Test)
Activity <- rbind(Activity_Train, Activity_Test)
setnames(Subject, "V1", "Subject")
setnames(Activity, "V1", "Activity")
Subject <- cbind(Subject, Activity)
Data_Merge <- rbind(Train, Test)
Data_Merge <- cbind(Subject, Data_Merge)
Data_Merge <- data.table(Data_Merge)
setkey(Data_Merge, Subject, Activity)
#Extract only the mean and standard deviation
Features <- read.table(file.path(Input, "features.txt"))
Features <- data.table(Features)
setnames(Features, "V1", "Feature")
setnames(Features, "V2", "Feature_Name")
Features <- Features[grep("-mean\\(\\)|-std\\(\\)", Feature_Name)]
Features$ID <- Features[, paste0("V", Feature)]
Data_Merge <- Data_Merge[, c(key(Data_Merge), Features$ID), with=FALSE]
#Name the activities
ActivityNames <- fread(file.path(Input, "activity_labels.txt"))
setnames(ActivityNames, "V1", "Activity")
setnames(ActivityNames, "V2", "Activity_Name")
Data_Merge <- merge(Data_Merge, ActivityNames, by="Activity", all.x=TRUE)
setkey(Data_Merge, Subject, Activity, Activity_Name)
#Create a second, independent tidy data set with the average of each variable for each activity and each subject
Data_Average <- aggregate(x=Data_Merge, by=list(Activity_Label=Data_Merge$Activity, Subject_Label=Data_Merge$Subject), FUN=mean)
Data_Average <- Data_Average[, !(colnames(Data_Average) %in% c("Activity_Label", "Subject_Label","Activity_Name"))]
Data_Average <- merge(Data_Average, ActivityNames, by="Activity", all.x=TRUE)
#Output results
write.table(Data_Merge, './Data_Merge.txt', row.names = F)
write.table(Data_Average, './Data_Average.txt', row.names = F)
