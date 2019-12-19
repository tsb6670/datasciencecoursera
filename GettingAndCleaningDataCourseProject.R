library(dplyr)

## activity names
df_activityNames <- read.csv("course3_wk4_data/UCI_HAR_Dataset/activity_labels.txt", header=FALSE, sep = " ")
colnames(df_activityNames) <- c("activity_id", "activity_name")
## create dplyr table for activity names
tbl_activityNames <- tbl_df(df_activityNames)
rm(df_activityNames)

## feature names
df_featureNames <- read.csv("course3_wk4_data/UCI_HAR_Dataset/features.txt", header=FALSE, sep = " ")
colnames(df_featureNames) <- c("feature_id", "feature_name")
## create dplyr table for feature names
tbl_featureNames <- tbl_df(df_featureNames)
rm(df_featureNames)

###
### Train data sets
###

## y_train data (measurements per row) to be linked to X_train
df_ytrain <- read.csv("course3_wk4_data/UCI_HAR_Dataset/train/y_train.txt", header=FALSE, sep = " ")
colnames(df_ytrain) <- c("activity_id")
## create dplyr table for y train data
tbl_ytrain <- tbl_df(df_ytrain)

## x_train data (measurements per row) to be linked with x_train
df_xtrain <- read.csv("course3_wk4_data/UCI_HAR_Dataset/train/X_train.txt", header=FALSE, sep = "", strip.white = TRUE, fill = TRUE)

## subject_train data - each row identifies the subject who performed the activity
df_subjecttrain <- read.csv("course3_wk4_data/UCI_HAR_Dataset/train/subject_train.txt", header=FALSE, sep = " ")
colnames(df_subjecttrain) <- c("subject_id")


## organize table merging fields
tbl <- arrange(inner_join(tbl_activityNames, tbl_ytrain, by = "activity_id"))
tbl["subject_id"] <- NA
tbl$subject_id <- df_subjecttrain$subject_id
tbl <- select(tbl, subject_id, activity_id, activity_name)

## create a subject/activity reference table for inertial signals
tbl_SubAct_train <- tbl

tbl["type"] <- "train"
tbl <- tbl_df(cbind(tbl, df_xtrain))

rm(df_ytrain)
rm(df_xtrain)
rm(df_subjecttrain)

###
### Train Inertial Signals Data Sets
###

### body_acc_x_train.txt
df_body_acc_x_train <- read.csv("course3_wk4_data/UCI_HAR_Dataset/train/Inertial\ Signals/body_acc_x_train.txt", header=FALSE, sep = "", strip.white = TRUE, fill = TRUE)
df_body_acc_x_train["type"] <- "body_acc_x_train"
tbl_body_acc_x_train <- select(tbl_df(df_body_acc_x_train), type, V1:V2)
names(tbl_body_acc_x_train)[2] <- "mean"
names(tbl_body_acc_x_train)[3] <- "std"
tbl_body_acc_x_train <- tbl_df(cbind(tbl_SubAct_train, tbl_body_acc_x_train))
## filter and add averages
tbl_body_acc_x_train <- mutate(filter(tbl_body_acc_x_train, !is.na(mean), !is.na(std)), avg_mean = mean(mean), avg_std = mean(std))

### body_acc_y_train.txt
df_body_acc_y_train <- read.csv("course3_wk4_data/UCI_HAR_Dataset/train/Inertial\ Signals/body_acc_y_train.txt", header=FALSE, sep = "", strip.white = TRUE, fill = TRUE)
df_body_acc_y_train["type"] <- "body_acc_y_train"
tbl_body_acc_y_train <- select(tbl_df(df_body_acc_y_train), type, V1:V2)
names(tbl_body_acc_y_train)[2] <- "mean"
names(tbl_body_acc_y_train)[3] <- "std"
tbl_body_acc_y_train <- tbl_df(cbind(tbl_SubAct_train, tbl_body_acc_y_train))
## filter and add averages
tbl_body_acc_y_train <- mutate(filter(tbl_body_acc_y_train, !is.na(mean), !is.na(std)), avg_mean = mean(mean), avg_std = mean(std))

### body_acc_z_train.txt
df_body_acc_z_train <- read.csv("course3_wk4_data/UCI_HAR_Dataset/train/Inertial\ Signals/body_acc_z_train.txt", header=FALSE, sep = "", strip.white = TRUE, fill = TRUE)
df_body_acc_z_train["type"] <- "body_acc_z_train"
tbl_body_acc_z_train <- select(tbl_df(df_body_acc_z_train), type, V1:V2)
names(tbl_body_acc_z_train)[2] <- "mean"
names(tbl_body_acc_z_train)[3] <- "std"
tbl_body_acc_z_train <- tbl_df(cbind(tbl_SubAct_train, tbl_body_acc_z_train))
## filter and add averages
tbl_body_acc_z_train <- mutate(filter(tbl_body_acc_z_train, !is.na(mean), !is.na(std)), avg_mean = mean(mean), avg_std = mean(std))

### body_gyro_x_train.txt
df_body_gyro_x_train <- read.csv("course3_wk4_data/UCI_HAR_Dataset/train/Inertial\ Signals/body_gyro_x_train.txt", header=FALSE, sep = "", strip.white = TRUE, fill = TRUE)
df_body_gyro_x_train["type"] <- "body_gyro_x_train"
tbl_body_gyro_x_train <- select(tbl_df(df_body_gyro_x_train), type, V1:V2)
names(tbl_body_gyro_x_train)[2] <- "mean"
names(tbl_body_gyro_x_train)[3] <- "std"
tbl_body_gyro_x_train <- tbl_df(cbind(tbl_SubAct_train, tbl_body_gyro_x_train))
## filter and add averages
tbl_body_gyro_x_train <- mutate(filter(tbl_body_gyro_x_train, !is.na(mean), !is.na(std)), avg_mean = mean(mean), avg_std = mean(std))

### body_gyro_y_train.txt
df_body_gyro_y_train <- read.csv("course3_wk4_data/UCI_HAR_Dataset/train/Inertial\ Signals/body_gyro_y_train.txt", header=FALSE, sep = "", strip.white = TRUE, fill = TRUE)
df_body_gyro_y_train["type"] <- "body_gyro_y_train"
tbl_body_gyro_y_train <- select(tbl_df(df_body_gyro_y_train), type, V1:V2)
names(tbl_body_gyro_y_train)[2] <- "mean"
names(tbl_body_gyro_y_train)[3] <- "std"
tbl_body_gyro_y_train <- tbl_df(cbind(tbl_SubAct_train, tbl_body_gyro_y_train))
## filter and add averages
tbl_body_gyro_y_train <- mutate(filter(tbl_body_gyro_y_train, !is.na(mean), !is.na(std)), avg_mean = mean(mean), avg_std = mean(std))

### body_gyro_z_train.txt
df_body_gyro_z_train <- read.csv("course3_wk4_data/UCI_HAR_Dataset/train/Inertial\ Signals/body_gyro_z_train.txt", header=FALSE, sep = "", strip.white = TRUE, fill = TRUE)
df_body_gyro_z_train["type"] <- "body_gyro_z_train"
tbl_body_gyro_z_train <- select(tbl_df(df_body_gyro_z_train), type, V1:V2)
names(tbl_body_gyro_z_train)[2] <- "mean"
names(tbl_body_gyro_z_train)[3] <- "std"
tbl_body_gyro_z_train <- tbl_df(cbind(tbl_SubAct_train, tbl_body_gyro_z_train))
## filter and add averages
tbl_body_gyro_z_train <- mutate(filter(tbl_body_gyro_z_train, !is.na(mean), !is.na(std)), avg_mean = mean(mean), avg_std = mean(std))

### total_acc_x_train.txt
df_total_acc_x_train <- read.csv("course3_wk4_data/UCI_HAR_Dataset/train/Inertial\ Signals/total_acc_x_train.txt", header=FALSE, sep = "", strip.white = TRUE, fill = TRUE)
df_total_acc_x_train["type"] <- "total_acc_x_train"
tbl_total_acc_x_train <- select(tbl_df(df_total_acc_x_train), type, V1:V2)
names(tbl_total_acc_x_train)[2] <- "mean"
names(tbl_total_acc_x_train)[3] <- "std"
tbl_total_acc_x_train <- tbl_df(cbind(tbl_SubAct_train, tbl_total_acc_x_train))
## filter and add averages
tbl_total_acc_x_train <- mutate(filter(tbl_total_acc_x_train, !is.na(mean), !is.na(std)), avg_mean = mean(mean), avg_std = mean(std))

### total_acc_y_train.txt
df_total_acc_y_train <- read.csv("course3_wk4_data/UCI_HAR_Dataset/train/Inertial\ Signals/total_acc_y_train.txt", header=FALSE, sep = "", strip.white = TRUE, fill = TRUE)
df_total_acc_y_train["type"] <- "total_acc_y_train"
tbl_total_acc_y_train <- select(tbl_df(df_total_acc_y_train), type, V1:V2)
names(tbl_total_acc_y_train)[2] <- "mean"
names(tbl_total_acc_y_train)[3] <- "std"
tbl_total_acc_y_train <- tbl_df(cbind(tbl_SubAct_train, tbl_total_acc_y_train))
## filter and add averages
tbl_total_acc_y_train <- mutate(filter(tbl_total_acc_y_train, !is.na(mean), !is.na(std)), avg_mean = mean(mean), avg_std = mean(std))

### total_acc_z_train.txt
df_total_acc_z_train <- read.csv("course3_wk4_data/UCI_HAR_Dataset/train/Inertial\ Signals/total_acc_z_train.txt", header=FALSE, sep = "", strip.white = TRUE, fill = TRUE)
df_total_acc_z_train["type"] <- "total_acc_z_train"
tbl_total_acc_z_train <- select(tbl_df(df_total_acc_z_train), type, V1:V2)
names(tbl_total_acc_z_train)[2] <- "mean"
names(tbl_total_acc_z_train)[3] <- "std"
tbl_total_acc_z_train <- tbl_df(cbind(tbl_SubAct_train, tbl_total_acc_z_train))
## filter and add averages
tbl_total_acc_z_train <- mutate(filter(tbl_total_acc_z_train, !is.na(mean), !is.na(std)), avg_mean = mean(mean), avg_std = mean(std))

###
### Test data sets
###

## y_test data (measurements per row) to be linked to X_train
df_ytest <- read.csv("course3_wk4_data/UCI_HAR_Dataset/test/y_test.txt", header=FALSE, sep = " ")
colnames(df_ytest) <- c("activity_id")
## create dplyr table for y train data
tbl_ytest <- tbl_df(df_ytest)

## x_ttest data (measurements per row) to be linked with x_test
df_xtest <- read.csv("course3_wk4_data/UCI_HAR_Dataset/test/X_test.txt", header=FALSE, sep = "", strip.white = TRUE, fill = TRUE)

## subject_test data - each row identifies the subject who performed the activity
df_subjecttest <- read.csv("course3_wk4_data/UCI_HAR_Dataset/test/subject_test.txt", header=FALSE, sep = " ")
colnames(df_subjecttest) <- c("subject_id")

tbl2 <- arrange(inner_join(tbl_activityNames, tbl_ytest, by = "activity_id"))
tbl2["subject_id"] <- NA
tbl2$subject_id <- df_subjecttest$subject_id
tbl2 <- select(tbl2, subject_id, activity_id, activity_name)

## create a subject/activity reference table for inertial signals
tbl2_SubAct_test <- tbl2

tbl2["type"] <- "test"
tbl2 <- tbl_df(cbind(tbl2, df_xtest))

rm(df_ytest)
rm(df_xtest)
rm(df_subjecttest)


###
### Test Inertial Signals Data Sets
###

### body_acc_x_test.txt
df_body_acc_x_test <- read.csv("course3_wk4_data/UCI_HAR_Dataset/test/Inertial\ Signals/body_acc_x_test.txt", header=FALSE, sep = "", strip.white = TRUE, fill = TRUE)
df_body_acc_x_test["type"] <- "body_acc_x_test"
tbl_body_acc_x_test <- select(tbl_df(df_body_acc_x_test), type, V1:V2)
names(tbl_body_acc_x_test)[2] <- "mean"
names(tbl_body_acc_x_test)[3] <- "std"
tbl_body_acc_x_test <- tbl_df(cbind(tbl2_SubAct_test, tbl_body_acc_x_test))
## filter and add averages
tbl_body_acc_x_test <- mutate(filter(tbl_body_acc_x_test, !is.na(mean), !is.na(std)), avg_mean = mean(mean), avg_std = mean(std))

### body_acc_x_test.txt
df_body_acc_y_test <- read.csv("course3_wk4_data/UCI_HAR_Dataset/test/Inertial\ Signals/body_acc_y_test.txt", header=FALSE, sep = "", strip.white = TRUE, fill = TRUE)
df_body_acc_y_test["type"] <- "body_acc_y_test"
tbl_body_acc_y_test <- select(tbl_df(df_body_acc_y_test), type, V1:V2)
names(tbl_body_acc_y_test)[2] <- "mean"
names(tbl_body_acc_y_test)[3] <- "std"
tbl_body_acc_y_test <- tbl_df(cbind(tbl2_SubAct_test, tbl_body_acc_y_test))
## filter and add averages
tbl_body_acc_y_test <- mutate(filter(tbl_body_acc_y_test, !is.na(mean), !is.na(std)), avg_mean = mean(mean), avg_std = mean(std))

### body_acc_z_test.txt
df_body_acc_z_test <- read.csv("course3_wk4_data/UCI_HAR_Dataset/test/Inertial\ Signals/body_acc_z_test.txt", header=FALSE, sep = "", strip.white = TRUE, fill = TRUE)
df_body_acc_z_test["type"] <- "body_acc_z_test"
tbl_body_acc_z_test <- select(tbl_df(df_body_acc_z_test), type, V1:V2)
names(tbl_body_acc_z_test)[2] <- "mean"
names(tbl_body_acc_z_test)[3] <- "std"
tbl_body_acc_z_test <- tbl_df(cbind(tbl2_SubAct_test, tbl_body_acc_z_test))
## filter and add averages
tbl_body_acc_z_test <- mutate(filter(tbl_body_acc_z_test, !is.na(mean), !is.na(std)), avg_mean = mean(mean), avg_std = mean(std))

### body_gyro_x_test.txt
df_body_gyro_x_test <- read.csv("course3_wk4_data/UCI_HAR_Dataset/test/Inertial\ Signals/body_gyro_x_test.txt", header=FALSE, sep = "", strip.white = TRUE, fill = TRUE)
df_body_gyro_x_test["type"] <- "body_gyro_x_test"
tbl_body_gyro_x_test <- select(tbl_df(df_body_gyro_x_test), type, V1:V2)
names(tbl_body_gyro_x_test)[2] <- "mean"
names(tbl_body_gyro_x_test)[3] <- "std"
tbl_body_gyro_x_test <- tbl_df(cbind(tbl2_SubAct_test, tbl_body_gyro_x_test))
## filter and add averages
tbl_body_gyro_x_test <- mutate(filter(tbl_body_gyro_x_test, !is.na(mean), !is.na(std)), avg_mean = mean(mean), avg_std = mean(std))

### body_gyro_y_test.txt
df_body_gyro_y_test <- read.csv("course3_wk4_data/UCI_HAR_Dataset/test/Inertial\ Signals/body_gyro_y_test.txt", header=FALSE, sep = "", strip.white = TRUE, fill = TRUE)
df_body_gyro_y_test["type"] <- "body_gyro_y_test"
tbl_body_gyro_y_test <- select(tbl_df(df_body_gyro_y_test), type, V1:V2)
names(tbl_body_gyro_y_test)[2] <- "mean"
names(tbl_body_gyro_y_test)[3] <- "std"
tbl_body_gyro_y_test <- tbl_df(cbind(tbl2_SubAct_test, tbl_body_gyro_y_test))
## filter and add averages
tbl_body_gyro_y_test <- mutate(filter(tbl_body_gyro_y_test, !is.na(mean), !is.na(std)), avg_mean = mean(mean), avg_std = mean(std))

### body_gyro_z_test.txt
df_body_gyro_z_test <- read.csv("course3_wk4_data/UCI_HAR_Dataset/test/Inertial\ Signals/body_gyro_z_test.txt", header=FALSE, sep = "", strip.white = TRUE, fill = TRUE)
df_body_gyro_z_test["type"] <- "body_gyro_z_test"
tbl_body_gyro_z_test <- select(tbl_df(df_body_gyro_z_test), type, V1:V2)
names(tbl_body_gyro_z_test)[2] <- "mean"
names(tbl_body_gyro_z_test)[3] <- "std"
tbl_body_gyro_z_test <- tbl_df(cbind(tbl2_SubAct_test, tbl_body_gyro_z_test))
## filter and add averages
tbl_body_gyro_z_test <- mutate(filter(tbl_body_gyro_z_test, !is.na(mean), !is.na(std)), avg_mean = mean(mean), avg_std = mean(std))

### total_acc_x_test.txt
df_total_acc_x_test <- read.csv("course3_wk4_data/UCI_HAR_Dataset/test/Inertial\ Signals/total_acc_x_test.txt", header=FALSE, sep = "", strip.white = TRUE, fill = TRUE)
df_total_acc_x_test["type"] <- "total_acc_x_test"
tbl_total_acc_x_test <- select(tbl_df(df_total_acc_x_test), type, V1:V2)
names(tbl_total_acc_x_test)[2] <- "mean"
names(tbl_total_acc_x_test)[3] <- "std"
tbl_total_acc_x_test <- tbl_df(cbind(tbl2_SubAct_test, tbl_total_acc_x_test))
## filter and add averages
tbl_total_acc_x_test <- mutate(filter(tbl_total_acc_x_test, !is.na(mean), !is.na(std)), avg_mean = mean(mean), avg_std = mean(std))

### total_acc_y_test.txt
df_total_acc_y_test <- read.csv("course3_wk4_data/UCI_HAR_Dataset/test/Inertial\ Signals/total_acc_y_test.txt", header=FALSE, sep = "", strip.white = TRUE, fill = TRUE)
df_total_acc_y_test["type"] <- "total_acc_y_test"
tbl_total_acc_y_test <- select(tbl_df(df_total_acc_y_test), type, V1:V2)
names(tbl_total_acc_y_test)[2] <- "mean"
names(tbl_total_acc_y_test)[3] <- "std"
tbl_total_acc_y_test <- tbl_df(cbind(tbl2_SubAct_test, tbl_total_acc_y_test))
## filter and add averages
tbl_total_acc_y_test <- mutate(filter(tbl_total_acc_y_test, !is.na(mean), !is.na(std)), avg_mean = mean(mean), avg_std = mean(std))

### total_acc_z_test.txt
df_total_acc_z_test <- read.csv("course3_wk4_data/UCI_HAR_Dataset/test/Inertial\ Signals/total_acc_z_test.txt", header=FALSE, sep = "", strip.white = TRUE, fill = TRUE)
df_total_acc_z_test["type"] <- "total_acc_z_test"
tbl_total_acc_z_test <- select(tbl_df(df_total_acc_z_test), type, V1:V2)
names(tbl_total_acc_z_test)[2] <- "mean"
names(tbl_total_acc_z_test)[3] <- "std"
tbl_total_acc_z_test <- tbl_df(cbind(tbl2_SubAct_test, tbl_total_acc_z_test))
## filter and add averages
tbl_total_acc_z_test <- mutate(filter(tbl_total_acc_z_test, !is.na(mean), !is.na(std)), avg_mean = mean(mean), avg_std = mean(std))


###
### Build the final dataset
###

## Extracting mean and standard deviation
ResTable_train <- select(tbl,  subject_id:V2)
names(ResTable_train)[5] <- "mean"
names(ResTable_train)[6] <- "std"
## filter and add averages
ResTable_train <- mutate(filter(ResTable_train, !is.na(mean), !is.na(std)), avg_mean = mean(mean), avg_std = mean(std))


ResTable_test  <- select(tbl2, subject_id:V2)
names(ResTable_test)[5] <- "mean"
names(ResTable_test)[6] <- "std"
## filter and add averages
ResTable_test <- mutate(filter(ResTable_test, !is.na(mean), !is.na(std)), avg_mean = mean(mean), avg_std = mean(std))


###
### Build the Data Set
###
ResTable <- bind_rows(ResTable_train, ResTable_test)

ResTable <- bind_rows(ResTable, tbl_body_acc_x_test)
ResTable <- bind_rows(ResTable, tbl_body_acc_y_test)
ResTable <- bind_rows(ResTable, tbl_body_acc_z_test)
ResTable <- bind_rows(ResTable, tbl_body_gyro_x_test)
ResTable <- bind_rows(ResTable, tbl_body_gyro_y_test)
ResTable <- bind_rows(ResTable, tbl_body_gyro_z_test)
ResTable <- bind_rows(ResTable, tbl_total_acc_x_test)
ResTable <- bind_rows(ResTable, tbl_total_acc_y_test)
ResTable <- bind_rows(ResTable, tbl_total_acc_z_test)

ResTable <- bind_rows(ResTable, tbl_body_acc_x_train)
ResTable <- bind_rows(ResTable, tbl_body_acc_y_train)
ResTable <- bind_rows(ResTable, tbl_body_acc_z_train)
ResTable <- bind_rows(ResTable, tbl_body_gyro_x_train)
ResTable <- bind_rows(ResTable, tbl_body_gyro_y_train)
ResTable <- bind_rows(ResTable, tbl_body_gyro_z_train)
ResTable <- bind_rows(ResTable, tbl_total_acc_x_train)
ResTable <- bind_rows(ResTable, tbl_total_acc_y_train)
ResTable <- bind_rows(ResTable, tbl_total_acc_z_train)

