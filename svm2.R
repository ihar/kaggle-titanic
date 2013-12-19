# Support Vector Machines and more accurate parameter tuning

library("caret")
load("data/clean_data.RData")
set.seed(2119)
training_rows <- createDataPartition(clean_train$Survived, p = 0.8, list = FALSE)

training <- clean_train[training_rows,-1] # without PassengerID
testing <- clean_train[-training_rows,-1]
train_control <- trainControl(method = "cv")

# 170 sigma-C pairs
tune_grid <- expand.grid(.sigma = 2^seq(-15, 1), .C=2^c(-5:4))

set.seed(2133)
svm_training2 <- train(Survived ~ Pclass + Sex + AgeScaled + 
                            SibSp + Parch + FareScaled + 
                            Embarked + Honorific + RelativeFareScales, 
                      data = training, 
                      method = "svmRadial",
                      trControl = train_control,
                      tuneGrid = tune_grid)

svm_training2
plot(svm_training2)
pred_training2 <- predict(svm_training2, testing)
cf <- confusionMatrix(pred_training2, testing$Survived)
cf$overall[1] # cv score 0.8157303, 0.78469 leaderboard score
# 0.79426 is the current best value in the LEaderboard

pred2 <- predict(svm_training2, clean_test)
save_prediction("svm3.csv", pred2)



# 186 sigma-C pairs
tune_grid4 <- expand.grid(.sigma = 2^seq(-15, 0, 0.5), .C=2^c(-1:4))

set.seed(2133)
svm_training4 <- train(Survived ~ Pclass + Sex + AgeScaled + 
                         SibSp + Parch + FareScaled + 
                         Embarked + Honorific + RelativeFareScales, 
                       data = training, 
                       method = "svmRadial",
                       trControl = train_control,
                       tuneGrid = tune_grid4)

svm_training4
pred_training4 <- predict(svm_training4, testing)
cf4 <- confusionMatrix(pred_training4, testing$Survived)
cf4$overall[1] # cv score 0.8157303
# 0.79426 is the current best value in the LEaderboard

# pred4 and pred2 are the same
# pred4 <- predict(svm_training4, clean_test)
# save_prediction("svm4.csv", pred4)
