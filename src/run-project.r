library('ProjectTemplate')
load.project()

train.labels <- as.factor(clean.train$survived)
rf.model <- randomForest(x = clean.train[,-1], 
                         y = train.labels, 
                         ntree = 10000, 
                         do.trace = T)
rf.predictions <- predict(rf.model, clean.test)
write((0:1)[rf.predictions], file = "rf02.predictions.csv", ncolumns = 1) 