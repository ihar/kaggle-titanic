# Data preprocessing

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
ctest.median.age <- median(test$age, na.rm = T)
ctrain.median.age <- median(train$age, na.rm = T)
clean.train$age[is.na(clean.train$age)] <- ctrain.median.age
clean.test$age[is.na(clean.test$age)] <- ctest.median.age

# Convert factors to integers
clean.train$sex <- as.integer(factor(clean.train$sex))
clean.test$sex <- as.integer(factor(clean.test$sex))
clean.train$embarked <- as.integer(factor(clean.train$embarked))
clean.test$embarked <- as.integer(factor(clean.test$embarked))
