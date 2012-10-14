library('ProjectTemplate')
load.project()

train.labels <- as.factor(clean.train$survived)
classwt <- c(sum(train.labels==0)/nrow(clean.train), sum(train.labels==1)/nrow(clean.train))
rf.model <- randomForest(x = clean.train[,-1], 
                         y = train.labels, 
                         classwt = classwt,
                         strata = train.labels,
                         ntree = 5000, 
                         do.trace = T)
rf.predictions <- predict(rf.model, clean.test)
write((0:1)[rf.predictions], file = "rf04.predictions.csv", ncolumns = 1) 

# Модель проверяем на исходных данных, которые из train
# Делим их в пропорции 8:2 на обучающие и тестовые.
# set.seed(12)
# split.set <- runif(nrow(train))
# to.train <- split.set > 0.2
# 
# cl.tr <- clean.train[to.train,]
# cl.tst <- clean.train[!to.train,]
# rf.model.2 <- randomForest(x = cl.tr[,-1], 
#                          y = factor(cl.tr[,1]),
#                          classwt <- c(1/sum(cl.tr[,1]==0), 1/sum(cl.tr[,1]==1))*nrow(cl.tr),
#                          xtest = cl.tst[,-1],
#                          ytest = factor(cl.tst[,1]),
#                          strata = factor(cl.tr[,-1]),
#                          ntree = 1500, 
#                          do.trace = F)
# rf.model.2$confusion