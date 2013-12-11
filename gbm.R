# Generalized Boosted Regression Models 

library("caret")
load("data/clean_data.RData")
set.seed(2242)
training_rows <- createDataPartition(clean_train$Survived, p = 0.8, list = FALSE)

training <- clean_train[training_rows,]
testing <- clean_train[-training_rows,]
train_control <- trainControl(method = "repeatedcv",
                              number = 10,
                              repeats = 10)
tune_grid <- createGrid("gbm", len = 7)

set.seed(2133)
# cv 0.852 / 0.8418
gbm_training <- train(Survived ~ Pclass + Sex + Age + 
                                  SibSp + Parch + Fare + 
                                  Embarked + Honorific, 
                      data = training, 
                      method = "gbm",
                      trControl = train_control,
                      tuneGrid = tune_grid,
                      verbose = FALSE)

# cv 0.85 / 0.8079
# gbm_training <- train(Survived ~ Pclass + Sex + Age + 
#                                   SibSp + Parch + Fare + 
#                                   Embarked + Honorific + RelativeFare, 
#                       data = training, 
#                       method = "gbm",
#                       trControl = train_control,
#                       tuneGrid = tune_grid,
#                       verbose = FALSE)


gbm_training
plot(gbm_training)
pred_training <- predict(gbm_training, testing)
confusionMatrix(pred_training, testing$Survived) 

# pred <- predict(gbm_training, clean_test)
# save_prediction("gbm1.csv", pred)

# set.seed(2133)
# gbm_training2 <- train(Survived ~ Pclass + Sex + Age + 
#                         SibSp + Parch + Fare + 
#                         Embarked + Honorific, 
#                       data = clean_train, 
#                       method = "gbm",
#                       trControl = train_control,
#                       tuneGrid = tune_grid,
#                       verbose = FALSE)
# gbm_training2
# pred2 <- predict(gbm_training2, clean_test)
# save_prediction("gbm2.csv", pred)

set.seed(2133)
# cv 0.85 / 0.8070
gbm_training3 <- train(Survived ~ . - PassengerId, 
                      data = training, 
                      method = "gbm",
                      trControl = train_control,
                      tuneGrid = tune_grid,
                      verbose = FALSE)
gbm_training3
plot(gbm_training3)
pred_training3 <- predict(gbm_training3, testing)
confusionMatrix(pred_training3, testing$Survived) 
