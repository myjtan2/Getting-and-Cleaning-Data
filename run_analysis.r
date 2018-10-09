## Coursera tidy data project: create one R script called run_analysis.R that does the following
##1.Merges the training and the test sets to create one data set.
##2 Extracts only the measurements on the mean and standard deviation for each measurement. 
##3 Uses descriptive activity names to name the activities in the data set
##4 Appropriately labels the data set with descriptive variable names. 
##5 From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

##Note that I filter before merging data

##install.packages("reshape2")
library(reshape2)


## Read all the separate but relevant data files###########

activity_labels <- read.table("./getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset/activity_labels.txt")

features <- read.table("./getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset/features.txt")

subject_test <- read.table("./getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset/test/subject_test.txt")

subject_train <- read.table("./getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset/train/subject_train.txt")

X_test <- read.table("./getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset/test/X_test.txt")

y_test <- read.table("./getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset/test/y_test.txt")

X_train <- read.table("./getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset/train/X_train.txt")

y_train <- read.table("./getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset/train/y_train.txt")

## Finish reading the relevant files.
###########################################################


##Create the test dataset with all the relevant names#########
names(X_test) <- features[,2]

##create a logical vector indicating only mean and std deviation
logical_mean_std_features <- grepl("mean|std", features[,2])

X_test<-X_test[,logical_mean_std_features]


##Put activity label next to activity number in y_test
y_test[,2] <- activity_labels[y_test[,1],2]

names(y_test) <- c("Activity_Num", "Activity_Label")

names(subject_test) <- "Human_Subject"

##Combine

test_data_set<-cbind(subject_test, y_test, X_test)
#############################################################

##Repeat for train set###############################
names(X_train) <- features[,2]

##create a logical vector indicating only mean and std deviation
logical_mean_std_features <- grepl("mean|std", features[,2])

X_train<-X_train[,logical_mean_std_features]


##Put activity label next to activity number in y_train
y_train[,2] <- activity_labels[y_train[,1],2]

names(y_train) <- c("Activity_Num", "Activity_Label")

names(subject_train) <- "Human_Subject"
#############################################################

##Combine
train_data_set<-cbind(subject_train, y_train, X_train)


## Combine test and train data

merged_data <- rbind(test_data_set, train_data_set)

##Begin tidying merged data#############################

##1 Remove redundant column
merged_data$Activity_Num <- NULL

##2 Melt dataset to turn some pof the column names to variables

kept_labels <- c("Human_Subject","Activity_Label")

##determine the column names to collasp into variables
thrown_labels <- setdiff(colnames(merged_data), kept_labels) 

melt_data<-melt(merged_data, id = kept_labels, measure.vars = thrown_labels)

##write.table(melt_data, file = "./melt_data.txt")
################################################################

##Aggregrate(Average) according to human subject and activity######

summarized_data <- dcast(melt_data, Human_Subject + Activity_Label ~ variable, mean)


############################################

write.table(summarized_data, file = "./summarized_data.txt",row.name=FALSE)
