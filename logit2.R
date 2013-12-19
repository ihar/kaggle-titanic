# Logistic Regression

rm(list=ls(all=TRUE))

require("caret")
require("FSelector")

source("functions-prediction.R")
load("data/clean_data.RData")

set.seed(1126)
training_rows <- createDataPartition(clean_train$Survived, p = 0.8, list = FALSE)

clean_train2 <- clean_train[, !names(clean_train) %in% 
                              c("PassengerId", "AgeScaled", "FareScaled", "RelativeFareScales", "RelativeFare")]

training <- clean_train2[training_rows,]
testing <- clean_train2[-training_rows,]

# 10-fold cross-validation
tr_control <- trainControl(method = "cv")

# Accuracy 0.825
set.seed(1140)
logit1 <- train(Survived ~ ., 
                data = training, 
                method = "glm", 
                trControl=tr_control)

clean_train2 <- clean_train
clean_train2$Survived <- as.numeric(clean_train2$Survived)-1
evaluator <- function(subset) {
  k <- 10 # k-fold cross-validation
  splits <- runif(nrow(clean_train2))
  results = sapply(1:k, function(i) {
    test.idx <- (splits >= (i - 1) / k) & (splits < i / k)
    train.idx <- !test.idx
    test <- clean_train2[test.idx, , drop=FALSE]
    train <- clean_train2[train.idx, , drop=FALSE]
    
    glm_model <- glm(as.simple.formula(subset, "Survived"), data = train)
    
    delta <- test$Survived - predict(glm_model, test)
    error_rate <-  sum(delta*delta) / nrow(test)
    return(1-error_rate)

    return(accuracy)
  })
  return(mean(results))
}

# [1] "Pclass"             "SibSp"              "Parch"              "Fare"              
# [5] "Honorific"          "RelativeFare"       "AgeScaled"          "RelativeFareScales"
exhaustive.search(names(clean_train[-1]), evaluator)

# [1] "Pclass"             "Sex"                "Age"                "SibSp"             
# [5] "Parch"              "Fare"               "Embarked"           "Honorific"         
# [9] "RelativeFare"       "FareScaled"         "RelativeFareScales" "Survived" 
backward.search(names(clean_train[-1]), evaluator)

# [1] "Pclass"    "SibSp"     "Parch"     "Embarked"  "Honorific" "Survived"# 
forward.search(names(clean_train[-1]), evaluator)

# [1] "Pclass"     "Sex"        "SibSp"      "Parch"      "Embarked"   "Honorific"  "AgeScaled" 
# [8] "FareScaled" "Survived"  
hill.climbing.search(names(clean_train[-1]), evaluator)

# cv accuracy 0.825
set.seed(1140)
logit1 <- train(Survived ~ ., 
                data = clean_train, 
                method = "glm", 
                trControl=tr_control)

res11 <- predict(logit1, clean_test)
save_prediction("logit2.csv", res11)

# cv accuracy 0.82
set.seed(1140)
logit2 <- train(Survived ~ Pclass + SibSp + Parch + Fare + 
                            Honorific + RelativeFare + AgeScaled + RelativeFareScales, 
                data = clean_train, 
                method = "glm", 
                trControl=tr_control)

res12 <- predict(logit2, clean_test)
save_prediction("logit3.csv", res12)
