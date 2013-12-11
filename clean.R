#
# Pre-processing of data
#

all_data <- rbind(init_train[,-2], init_test)

# Fixing missing values if there are any
# Looking for NA values
apply(all_data, 2, function(x) length(which(is.na(x)))  )  
# 263 missing values in Age feature
# 1 missing value in Fare feature

# Looking for empty strings
apply(all_data, 2, function(x) length(which(nchar(x)==0))  )  
# 1014 empty values in Cabin feature
# 2 empty values in Embarked feature

# Replace missing value in Fare with the most frequent
all_data$Fare[is.na(all_data$Fare)] <- median(all_data$Fare, na.rm=TRUE)
# Replace empty values in Embarked with the most frequent, which is "S"
table(all_data$Embarked)
all_data$Embarked[all_data$Embarked == ""] <- "S"

# There are too many missing values in Cabin feature.
# We will drop that feature from dataframe
all_data$Cabin <- NULL
# Also Ticket seems to be not very useful.
all_data$Ticket <- NULL

# Instead of using median age of all passengers to restore missing Age values
# we will divide the passengers into groups according their titles (Mr., Mrs., etc.)
# And then we will take the median age for each group to fill in the missing ages
all_data$Honorific <- sapply(all_data$Name, get_honorific, USE.NAMES=FALSE)
# Master, Miss, Mr, Mrs
all_data$Name <- NULL

# Fill in empty Age values with most popular honorific ages
master_data <- subset(all_data, Honorific == "Master")
master_mean_age <- mean(master_data$Age, na.rm=TRUE)

mr_data <- subset(all_data, Honorific == "Mr")
mr_mean_age <- mean(mr_data$Age, na.rm=TRUE)

miss_data <- subset(all_data, Honorific == "Miss")
miss_mean_age <- mean(miss_data$Age, na.rm=TRUE)

mrs_data <- subset(all_data, Honorific == "Mrs")
mrs_mean_age <- mean(mrs_data$Age, na.rm=TRUE)

all_data$Age[(all_data$Honorific=="Mr")&(is.na(all_data$Age))] <- mr_mean_age
all_data$Age[(all_data$Honorific=="Master")&(is.na(all_data$Age))] <- master_mean_age
all_data$Age[(all_data$Honorific=="Miss")&(is.na(all_data$Age))] <- miss_mean_age
all_data$Age[(all_data$Honorific=="Mrs")&(is.na(all_data$Age))] <- mrs_mean_age

# Maybe those who pay for a ticket less than the average less likely to survive. 
# Let's create a new variable to follow this idea
# Three classes reflects socio-economic status of passengers.
# Find average fare for each class
pclass1 <- subset(all_data, Pclass == 1)
pclass1_mean_fare <- mean(pclass1$Fare)
pclass2 <- subset(all_data, Pclass == 2)
pclass2_mean_fare <- mean(pclass2$Fare)
pclass3 <- subset(all_data, Pclass == 3)
pclass3_mean_fare <- mean(pclass3$Fare)

all_data$RelativeFare <- NA

all_data$RelativeFare[all_data$Pclass==1] <- all_data$Fare[all_data$Pclass==1] - pclass1_mean_fare
all_data$RelativeFare[all_data$Pclass==2] <- all_data$Fare[all_data$Pclass==2] - pclass2_mean_fare
all_data$RelativeFare[all_data$Pclass==3] <- all_data$Fare[all_data$Pclass==3] - pclass3_mean_fare

# Categorical variables to factor
all_data$Pclass <- as.factor(all_data$Pclass)
all_data$Sex <- as.factor(all_data$Sex)
all_data$Embarked <- as.factor(all_data$Embarked)
all_data$Honorific <- as.factor(all_data$Honorific)

# Scale numerical features
all_data$AgeScaled <- scale_feature(all_data$Age)
all_data$FareScaled <- scale_feature(all_data$Fare)
all_data$RelativeFareScales <- scale_feature(all_data$RelativeFare)

# Split combined data back into test set and training set
train_rows <- 1:nrow(init_train)
clean_train <- all_data[train_rows,]
clean_train$Survived <- as.factor(init_train$Survived)
clean_test <- all_data[-train_rows,]

save(clean_train, clean_test, file = "data/clean_data.RData")

