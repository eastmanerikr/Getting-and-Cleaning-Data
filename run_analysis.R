## 1. Merges the training and the test sets to create one data set.
## 2. Extracts only the measurements on the mean and standard deviation for each measurement.
## 3. Uses descriptive activity names to name the activities in the data set
## 4. Appropriately labels the data set with descriptive variable names.
## 5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

##Import Data Sets and Transforming to facilitate merge

library(dplyr)


##Data below applies to both Training and Test sets. 
Activity_Labels <- read.table("C:/Users/ee60700/Documents/Coursera/Getting and Cleaning Data/UCI HAR Dataset/activity_labels.txt", col.names = c("Activity_Code","Activity_Name"))
Features <- read.table("C:/Users/ee60700/Documents/Coursera/Getting and Cleaning Data/UCI HAR Dataset/features.txt", col.names = c("Feature_Code","Feature_Name"))

## Read in Training Set
Subject_Train <- read.table("C:/Users/ee60700/Documents/Coursera/Getting and Cleaning Data/UCI HAR Dataset/train/subject_train.txt", col.names = c("Subject_Number"))
Subject_Train <- Subject_Train %>% mutate(Subject_Data_Set = "Train")
## Columns in x_train and y_train tables below are now named by the Feature Names provide in documentation "features.txt" by the original author.
y_train <- read.table("C:/Users/ee60700/Documents/Coursera/Getting and Cleaning Data/UCI HAR Dataset/train/y_train.txt", col.names = c("Activity_Code"))
x_train <- read.table("C:/Users/ee60700/Documents/Coursera/Getting and Cleaning Data/UCI HAR Dataset/train/X_train.txt", col.names = Features$Feature_Name)

##Test Set
Subject_Test <- read.table("C:/Users/ee60700/Documents/Coursera/Getting and Cleaning Data/UCI HAR Dataset/test/subject_test.txt", col.names = c("Subject_Number"))
Subject_Test <- Subject_Test %>% mutate(Subject_Data_Set = "Test")

y_test <- read.table("C:/Users/ee60700/Documents/Coursera/Getting and Cleaning Data/UCI HAR Dataset/test/y_test.txt", col.names = c("Activity_Code"))
x_test <- read.table("C:/Users/ee60700/Documents/Coursera/Getting and Cleaning Data/UCI HAR Dataset/test/X_test.txt", col.names = Features$Feature_Name)

##Join Activity Name to y_test and y_train. Joining by common name "Activity_Code"
y_test <- y_test %>% left_join(Activity_Labels) %>% select(Activity_Name)
y_train <- y_train %>% left_join(Activity_Labels) %>% select(Activity_Name)

##Create Full Training and Test data sets

Full_Train_Set <- cbind.data.frame(Subject_Train, y_train, x_train)
Full_Test_Set <- cbind.data.frame(Subject_Test, y_test, x_test)

##Create Full Data Set

Full_Data_Set <- rbind.data.frame(Full_Train_Set, Full_Test_Set)

##Extract Mean and SD for each Measurement. Selected Features must contain the word mean() or std()

Mean_SD_Data_Set <- Full_Data_Set %>% select(Subject_Number, Subject_Data_Set, Activity_Name, contains("mean"), contains("std"))

##Create the data set described in step 5. The mean for each Feature (Mean & SD Only outlined in Steps 1-4) 
##Adds the word "mean" after each Feature Name for the column headers.

Averages_Data_Set <- Mean_SD_Data_Set %>% select(-Subject_Data_Set) %>%
                     group_by(Subject_Number, Activity_Name) %>%
                     dplyr::summarise_all(list(mean = mean)) ##Applies the function mean() to all variables in the table


                  
