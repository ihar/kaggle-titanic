source("functions-prediction.R")
require("caret")
load("data/clean_data.RData")

set.seed(2252)
training_rows <- createDataPartition(clean_train$Survived, p = 0.8, list = FALSE)

training <- clean_train[training_rows, -1]
testing <- clean_train[-training_rows, -1]

train_control <- trainControl(method = "cv")

#leaderboard: 
# 0.78947 leaderboard, cv score 0.833
# 0.79426 leaderboard, cv score 0.84

tune_grid <- expand.grid(.mtry=2:12)

set.seed(2020)
rf_training5 <- train(Survived ~., 
                      data = training, 
                      method = "rf",
                      trControl = train_control,
                      tuneGrid = tune_grid)
rf_training5

pred_training <- predict(rf_training5, testing)
conf_m <- confusionMatrix(pred_training, testing$Survived) 
conf_m$overall[1] 

pred <- predict(rf_training5, clean_test)
save_prediction("rf6.csv", pred)

set.seed(2020)
rf_training61 <- train(Survived ~., 
                       data = training, 
                       method = "rf",
                       trControl = train_control,
                       tuneGrid = tune_grid, 
                       ntree = 1000)
set.seed(2020)
rf_training62 <- train(Survived ~., 
                      data = training, 
                      method = "rf",
                      trControl = train_control,
                      tuneGrid = tune_grid, 
                      ntree = 1500)

rf_training61
rf_training62

pred_training <- predict(rf_training62, testing)
conf_m <- confusionMatrix(pred_training, testing$Survived) 
conf_m$overall[1] 

pred <- predict(rf_training61, clean_test)
save_prediction("rf61.csv", pred)


