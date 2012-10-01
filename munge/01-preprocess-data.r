# Data preprocessing

# I don't see why names of passenger should be important in this competition.
# Number of ticket is  also meaningless here.
# Cabin seems to be important but I plan to use it later
# (A is upper desk, G is lower; upper desk is better for survival)
drop.columns <- c("name", "Family", "Name", "ticket", "cabin")
train <- train[, !(names(train) %in% drop.columns)]
test <- test[, !(names(test) %in% drop.columns)]

# Dealing with NA in columns Age (both in test and train sets)
# and Fare (in test set, test[153,])

