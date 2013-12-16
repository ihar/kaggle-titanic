# Data exploration

source("load.r")

require("ggplot2")

init_train$Survived <- as.factor(init_train$Survived)
init_train$Sex <- as.factor(init_train$Sex)
init_train$Pclass <- as.factor(init_train$Pclass)


qplot(Survived, data = init_train, geom="bar", fill = Sex)
table(init_train$Sex, init_train$Survived)

qplot(factor(Age), data = init_train[complete.cases(init_train),], geom="bar", fill = Survived)
qplot(Pclass, data = init_train[complete.cases(init_train),], geom="bar", fill = Survived)

