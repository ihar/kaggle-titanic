# Logistic Regression

require("caret")

load("data/clean_data.RData")

# 10-fold cross-validation
tr_control <- trainControl(method = "repeatedcv", number = 10, repeats = 10)
  
logit1 <- train(Survived ~ Pclass + Age + SibSp + Parch + Embarked + Honorific, 
                data = clean_train, 
                method = "glm", 
                trControl=tr_control)

# CV accuracy 0.829
logit1

res1 <- predict(logit1, clean_test)
save_prediction("logit1.csv", res1)

