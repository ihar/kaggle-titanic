#
# Loading train and test data
#

# 891 objects of 12 variables
init_train <- read.csv("data/train.csv", stringsAsFactor=FALSE)

# 418 objects of 11 variables
init_test <- read.csv("data/test.csv", stringsAsFactor=FALSE)

# Target variable is "Survived"
