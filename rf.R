# Random Forests

require("caret")
load("data/clean_data.RData")

set.seed(2252)
training_rows <- createDataPartition(clean_train$Survived, p = 0.8, list = FALSE)

training <- clean_train[training_rows,]
testing <- clean_train[-training_rows,]

train_control <- trainControl(method = "repeatedcv",
                              number = 5,
                              repeats = 5)

tune_grid <- createGrid("rf", len = 4, data = training)

# I didn't set random seed here. 
# So the resulting submission file may differ from file in project repository
rf_training <- train(Survived ~., 
                      data = training, 
                      method = "rf",
                      trControl = train_control,
                      tuneGrid = tune_grid,
                      importance = TRUE)
rf_training
varImp(rf_training)

pred_training <- predict(rf_training, testing)
confusionMatrix(pred_training, testing$Survived) 
# Accuracy 0.8136

# Didn't set random seed
rf1 <- train(Survived ~., 
             data = clean_train, 
             method = "rf",
             trControl = train_control,
             tuneGrid = tune_grid)
pred1 <- predict(rf1, clean_test)
save_prediction("rf1.csv", pred1)

pred2 <- predict(rf_training, clean_test)
save_prediction("rf2.csv", pred2)

####

# Make more trees (1500  instead of default 500)
set.seed(3)
rf_training2 <- train(Survived ~., 
                     data = training, 
                     method = "rf",
                     trControl = train_control,
                     tuneGrid = tune_grid,
                     ntree = 1500)
rf_training2

pred_training2 <- predict(rf_training2, testing)
confusionMatrix(pred_training2, testing$Survived)  # 0.8023

pred3 <- predict(rf_training2, clean_test)
save_prediction("rf3.csv", pred3)

# Make more trees (1500  instead of default 500)
set.seed(3)
rf_training3 <- train(Survived ~. - PassengerId, 
                      data = training, 
                      method = "rf",
                      trControl = train_control,
                      tuneGrid = tune_grid,
                      ntree = 1500)
rf_training3

pred_training3 <- predict(rf_training3, testing)
confusionMatrix(pred_training3, testing$Survived)  # 0.8644, but 0.78 on the leaderboard

pred31 <- predict(rf_training3, clean_test)
save_prediction("rf4.csv", pred31)
