# Getting-and-Cleaning-Data-Course-Project

## Purpose
To prepare tidy data that can be used for later analysis. 

## Files Description
The files in this repository include: 

1) a tidy data set as described below

2) a code book that describes the variables, the data, and any transformations or work that was performed to clean up the data called CodeBook.md. 

3) a README.md explaining how all of the scripts work and how they are connected

4) R script called run_analysis.R that does the following:

  >  Merges the training and the test sets to create one data set.

  >  Extracts only the measurements on the mean and standard deviation for each measurement.

  >  Uses descriptive activity names to name the activities in the data set.

  >  Appropriately labels the data set with descriptive variable names.

  >  From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

To use the R script, the folder "UCI HAR Dataset" including the original data should be under the working directory of R.

## Data Source

One of the most exciting areas in all of data science right now is wearable computing - see for example this article . Companies like Fitbit, Nike, and Jawbone Up are racing to develop the most advanced algorithms to attract new users. The data linked to from the course website represent data collected from the accelerometers from the Samsung Galaxy S smartphone. A full description is available at the site where the data was obtained:

http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones

The data for the project:

https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip

## Instruction

### 1. Download the data zip file above and extract it into the folder "UCI HAR Dataset" under the working directory of R

### 2. Run the "run_analysis.R" script in R

### 3. Find the output file under working directory named "Tidy_Data_Average.txt"
