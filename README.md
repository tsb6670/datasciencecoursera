The analysis files include: 

(1) Readme_GettingandCleaningDataCourseProject.txt. This file is the project readme.
(2) CodeBook_GettingandCleaningDataCourseProject.txt. This file describes each variable in the dataset.
(3) course3_wk4_data/UCI_HAR_dataset. This directory contains the project source data unmodified.
(4) GettingandCleaningDataCourseProject.R - the source code for this project producing the final dataset.
(5) course3_wk4_data/GettingandCleaningDataCourseProject.csv - comma separated value result set
(6) course3_wk4_data/GettingandCleaningDataCourseProject.txt - text variant of dataset

Note that the variable classes within Inertial Signals for both train and test map to feate_info.txt datapoints where the first two values in the raw data are the mean and the standard deviation. The same is true for the train and test summary data (X_test.txt and X_train.txt). Raw datafiles are whitespace delimited and require additional parameters to read.csv in order to parse correctly. For example:

df_xtrain <- read.csv("course3_wk4_data/UCI_HAR_Dataset/train/X_train.txt", header=FALSE, sep = "", strip.white = TRUE, fill = TRUE)

