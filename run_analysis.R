#Load required packages
library(data.table)
library(reshape2)

#Load data
Input <- file.path(getwd(), "UCI HAR Dataset")
Subject_Train <- read.table(file.path(Input, "train", "subject_train.txt"))
Subject_Test  <- read.table(file.path(Input, "test" , "subject_test.txt" ))
Activity_Train <- read.table(file.path(Input, "train", "Y_train.txt"))
Activity_Test  <- read.table(file.path(Input, "test" , "Y_test.txt" ))
Train <- read.table(file.path(Input, "train", "X_train.txt"))
Test <- read.table(file.path(Input, "test", "X_test.txt"))
Features <- read.table(file.path(Input, "features.txt"))
Features <- data.table(Features)
ActivityNames <- read.table(file.path(Input, "activity_labels.txt"))
ActivityNames <- data.table(ActivityNames)

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
setnames(Features, "V1", "Feature")
setnames(Features, "V2", "Feature_Name")
Features <- Features[grep("-mean\\(\\)|-std\\(\\)", Feature_Name)]
Features$ID <- Features[, paste0("V", Feature)]
Data_Merge <- Data_Merge[, c(key(Data_Merge), Features$ID), with=FALSE]

#Name the activities
setnames(ActivityNames, "V1", "Activity")
setnames(ActivityNames, "V2", "Activity_Name")
Data_Merge <- merge(ActivityNames, Data_Merge, by="Activity", all.x=TRUE)
setkey(Data_Merge, Subject, Activity_Name)

#Create a second, independent tidy data set with the average of each variable for each activity and each subject
Data_Average <- aggregate(x=Data_Merge, by=list(Activity_Label=Data_Merge$Activity, Subject_Label=Data_Merge$Subject), FUN=mean)
Data_Average <- Data_Average[, !(colnames(Data_Average) %in% c("Activity_Label", "Subject_Label","Activity_Name"))]
Data_Average <- merge(ActivityNames, Data_Average, by="Activity", all.x=TRUE)

#New naming
rename <- data.table(c("Activity_Name","Acticity","Subject"))
setnames(rename, "V1", "Feature_Name")
rename <- rbind(rename,Features[, 2])
rename <- as.character(unlist(rename))
names(Data_Merge) <- rename
names(Data_Average) <- rename
Tidy_Data <- Data_Merge[, -1]
Tidy_Data_Average <- Data_Average[, -1]

#Output results
write.table(Tidy_Data, './Tidy_Data.txt', row.names = F)
write.table(Tidy_Data_Average, './Tidy_Data_Average.txt', row.names = F)
