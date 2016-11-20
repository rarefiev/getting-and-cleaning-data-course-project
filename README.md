# Overview #
This repo contains run_analysis.R script, which creates tidy dataset from HAR Dataset. And description of resulting dataset and the process of its creation called CodeBook.md

# How to use scripts #
1. Download and unzip HAR dataset from https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip
2. Put 'UCI HAR Dataset' folder into the same directory with run_analysis.R script.
3. Run run_analysis.R script:
    `$ R -f run_analysis.R --slave`
4. output.txt with resulted data set will be created.

# The repo includes the following files: #

- README.md - this file
- CodeBook.md - description of data processing
- run_analysis.R - script to make resulting data set.

# Prerequisites #
Library dplyr should be installed. https://cran.r-project.org/web/packages/dplyr/README.html