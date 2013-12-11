# Support Vector Machines

library("caret")
load("data/clean_data.RData")
set.seed(2119)
training_rows <- createDataPartition(clean_train$Survived, p = 0.8, list = FALSE)

training <- clean_train[training_rows,]
testing <- clean_train[-training_rows,]
train_control <- trainControl(method = "repeatedcv",
                              number = 5,
                              repeats = 5)
tune_grid <- createGrid("svmRadial", len = 4)

set.seed(2133)
# 0.8192
# Without PassengerID!!!
svm_training2 <- train(Survived ~ Pclass + Sex + AgeScaled + 
                                  SibSp + Parch + FareScaled + 
                                  Embarked + Honorific + RelativeFareScales, 
                      data = training, 
                      method = "svmRadial",
                      trControl = train_control,
                      tuneGrid = tune_grid)

# 0.8136
# svm_training2 <- train(Survived ~ Pclass + Sex + AgeScaled + 
#                                   Parch + FareScaled + 
#                                   Honorific + RelativeFareScales, 
#                       data = training, 
#                       method = "svmRadial",
#                       trControl = train_control,
#                       tuneGrid = tune_grid)

# 0.8079
# svm_training2 <- train(Survived ~ Pclass + Sex + AgeScaled + 
#                                   Parch + FareScaled + Honorific, 
#                       data = training, 
#                       method = "svmRadial",
#                       trControl = train_control,
#                       tuneGrid = tune_grid)
svm_training2
pred_training2 <- predict(svm_training2, testing)
confusionMatrix(pred_training2, testing$Survived) 

pred2 <- predict(svm_training2, clean_test)
save_prediction("svm1.csv", pred2)

set.seed(2235)
svm_training3 <- train(Survived ~ Pclass + Sex + AgeScaled + 
                                  Parch + FareScaled + Honorific, 
                      data = clean_train, 
                      method = "svmRadial",
                      trControl = train_control,
                      tuneGrid = tune_grid)
svm_training3
plot(svm_training3)
svm_training3$bestTune
pred3 <- predict(svm_training3, clean_test)
save_prediction("svm2.csv", pred3)
