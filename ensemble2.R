# Ensembles

rm(list = ls(all = TRUE))
source("functions-prediction.R")
require("caretEnsemble")
load("data/clean_data.RData")

set.seed(22)
training_rows <- createDataPartition(clean_train$Survived, p = 0.8, list = FALSE)

# Because caretEnsemble works only with RMSE
clean_train$Survived <- as.integer(clean_train$Survived)-1

training <- clean_train[training_rows, -1]
testing <- clean_train[-training_rows, -1]

gbm_tune_grid <- createGrid("gbm", len = 7)
rf_tune_grid <- createGrid("rf", len = 7, data = training)
svm_tune_grid <- createGrid("svmRadial", len = 7)

tr_control <- trainControl(method = "cv", 
                              classProbs = TRUE,
                              returnData = FALSE,
                              returnResamp = "none",
                              savePredictions = TRUE,
                              index=createMultiFolds(training$Survived, k=10, times=1))

logit_model <- train(Survived ~ ., 
                    data = training, 
                    method = "glm", 
                    trControl=tr_control)

gbm_model <- train(Survived ~ ., 
                   data = training, 
                   method = "gbm",
                   trControl = tr_control,
                   tuneGrid = gbm_tune_grid,
                   verbose = FALSE)

rf_model <- train(Survived ~., 
                     data = training, 
                     method = "rf",
                     trControl = tr_control,
                     tuneGrid = rf_tune_grid)

svm_model <- train(Survived ~ Pclass + Sex + AgeScaled + 
                         SibSp + Parch + FareScaled + 
                         Embarked + Honorific + RelativeFareScales, 
                       data = training, 
                       method = "svmRadial",
                       trControl = tr_control,
                       tuneGrid = svm_tune_grid)

all_models <- list(logit_model, gbm_model, rf_model, svm_model)
names(all_models) <- sapply(all_models, function(x) x$method)
sort(sapply(all_models, function(x) min(x$results$RMSE)))

# gbm        rf       glm svmRadial 
# 0.3472557 0.3512593 0.3646367 0.3765421
greedy <- caretEnsemble(all_models, iter = 1000L)
# gbm svmRadial        rf       glm 
# 0.633     0.147     0.134     0.086 
sort(greedy$weights, decreasing = TRUE)
#      RMSE 
# 0.3478996 
greedy$error 

#Make a linear regression ensemble
linear <- caretStack(all_models, method='glm', trControl=trainControl(method='cv'))
summary(linear$ens_model$finalModel)
linear$error #0.3489195

preds <- data.frame(sapply(all_models, predict, newdata=testing))
preds$ENS_greedy <- predict(greedy, newdata=testing)
preds$ENS_linear <- predict(linear, newdata=testing)
sort(sqrt(colMeans((preds - testing$Survived) ^ 2)))

preds2 <- data.frame(sapply(all_models, predict, newdata=clean_test))
preds2$ENS_greedy <- predict(greedy, newdata=clean_test)
preds2$ENS_linear <- predict(linear, newdata=clean_test)

# Determine threshold
for (i in seq(0.1, 0.9, 0.05)) {
  cur_pred <- ifelse(preds$ENS_greedy > i, 1, 0)
  print(paste(i, sum(abs(cur_pred - testing$Survived))))
}

pred01 <- ifelse(preds2$ENS_greedy > 0.55, 1, 0)
# save_prediction("ensemble5.csv", pred01)
