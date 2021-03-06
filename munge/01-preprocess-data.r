# Data preprocessing

# I don't see why names of passenger should be important in this competition.
# Number of ticket is  also meaningless here.
# Cabin seems to be important but I plan to use it later
# (A is upper desk, G is lower; upper desk is better for survival)
drop.columns <- c("name", "ticket", "cabin")
clean.train <- train[, !(names(train) %in% drop.columns)]
clean.test <- test[, !(names(test) %in% drop.columns)]

# Dealing with NA in columns Age (both in test and train sets)
# and Fare (in test set, test[153,]) and Embarked (in train set, train[c(62, 830),])

# Missed fares replaced by median value of that class
ctrain.pclass.medians <- tapply(clean.train$fare, clean.train$pclass, median, na.rm = T)
ctest.pclass.medians <- tapply(clean.test$fare, clean.test$pclass, median, na.rm = T)
# Look, look, a bit of govnokod is here!
clean.train$fare[is.na(clean.train$fare)] <- ctrain.pclass.medians[clean.train$pclass[which(is.na(clean.train$fare))]]
clean.test$fare[is.na(clean.test$fare)] <- ctest.pclass.medians[clean.test$pclass[which(is.na(clean.test$fare))]]

# Missing values in Embarked
ctest.embarked.most.frequent <- names(rev(sort(table(test$embarked)))[1])
ctrain.embarked.most.frequent <- names(rev(sort(table(train$embarked)))[1])
clean.train$embarked[clean.train$embarked == ""] <- ctrain.embarked.most.frequent
clean.test$embarked[clean.test$embarked == ""] <- ctest.embarked.most.frequent

# Missing values in Age
# Change it to values from (uniform or normal) distribution
set.seed(4)
ctest.median.age <- median(test$age, na.rm = T)
ctrain.median.age <- median(train$age, na.rm = T)

ctest.rnorm.ages <- floor(rnorm(length(which(is.na(test$age))), mean = ctest.median.age, sd = ctest.median.age))
ctrain.rnorm.ages <- floor(rnorm(length(which(is.na(train$age))), mean = ctrain.median.age, sd = ctrain.median.age))

clean.train$age[is.na(clean.train$age)] <- ctrain.rnorm.ages
clean.test$age[is.na(clean.test$age)] <- ctest.rnorm.ages

# Convert factors to integers
clean.train$sex <- as.integer(factor(clean.train$sex))
clean.test$sex <- as.integer(factor(clean.test$sex))
clean.train$embarked <- as.integer(factor(clean.train$embarked))
clean.test$embarked <- as.integer(factor(clean.test$embarked))
