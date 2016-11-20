# Creates and saves in a file named 'output.txt' a tidy data set with averages of measurement per activity and subject.

library(dplyr) 

# create table with needed features only, based on features.txt and features_info.txt
features <- read.table(header = TRUE, text = "
colNum colName
1 tBodyAcc-mean()-X
2 tBodyAcc-mean()-Y
3 tBodyAcc-mean()-Z
4 tBodyAcc-std()-X
5 tBodyAcc-std()-Y
6 tBodyAcc-std()-Z
41 tGravityAcc-mean()-X
42 tGravityAcc-mean()-Y
43 tGravityAcc-mean()-Z
44 tGravityAcc-std()-X
45 tGravityAcc-std()-Y
46 tGravityAcc-std()-Z
")

baseDir <- './UCI HAR Dataset' # set base dir for data

loadAndMerge <- function(filename) {
    # loads both train and test parts of `filename` and merges them
    makeFilePath <- function(subset, filename) {
        file.path(baseDir, subset, paste(filename, '_', subset, '.txt', sep = ''))
    }
    train_df <- read.table(makeFilePath('train', filename))
    test_df <-  read.table(makeFilePath('test', filename))
    bind_rows(train_df, test_df)
}

# load measurements dataframe 
measurements <- loadAndMerge('X')

# load subjects dataframe 
subjects <- loadAndMerge('subject')

# load activities
activities <- loadAndMerge('y')

# select only measurements of the mean and standard deviation for tBodyAcc and tGravityAcc
selected_measurement <- select(measurements, features$colNum)

# add descriptive activity names
activity_names <- read.table(file.path(baseDir, "activity_labels.txt"), col.names = c('activitycode', 'activity'))
activities <- select(inner_join(activities, activity_names, by = c('V1' = 'activitycode')), activity)

# add descriptive column names
colnames(activities) <- c('activity')
colnames(subjects) <- c('subject')
colnames(selected_measurement) <- features$colName

# create common data set binding activities, selected measurements and subjects
df <- bind_cols(subjects, activities, selected_measurement)

# create resulting table 
result <- summarise_each(group_by(df, subject, activity), funs(mean))

# write the resulting data frame into file 'output.txt'
write.table(result, 'output.txt', row.names = FALSE)


