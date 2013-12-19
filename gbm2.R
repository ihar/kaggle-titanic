# Generalized Boosted Regression Models and parameter tuning

library("caret")
load("data/clean_data.RData")
set.seed(2242)
training_rows <- createDataPartition(clean_train$Survived, p = 0.8, list = FALSE)

training <- clean_train[training_rows,-1]
testing <- clean_train[-training_rows,-1]
train_control <- trainControl(method = "cv")
tune_grid3 <- expand.grid(.interaction.depth = seq(1, 9, 2),
                          .n.trees = seq(1, 12, 2)*100,
                          .shrinkage = 10^-c(1:4))

set.seed(2133)
gbm_training3 <- train(Survived ~ Pclass + Sex + Age + 
                                  SibSp + Parch + Fare + 
                                  Embarked + Honorific, 
                      data = training, 
                      method = "gbm",
                      trControl = train_control,
                      tuneGrid = tune_grid3,
                      verbose = FALSE)

gbm_training3
plot(gbm_training3)
pred_training <- predict(gbm_training3, testing)
confusionMatrix(pred_training, testing$Survived) # cv 0.8192

pred3 <- predict(gbm_training3, clean_test)
save_prediction("gbm3.csv", pred3)
