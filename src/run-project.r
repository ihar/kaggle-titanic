library('ProjectTemplate')
load.project()

err <- function(target, predict) (1- (1/length(target)) * sum(abs(target-predict)))

target <- train$survived
end_trn <- length(train$survived)

clean.train$survived <- NULL
train <- rbind(clean.train, clean.test)
end <- length(train$sex)

################### GBM model 1 settings, these can be varied
pr <- 0
tr <- 0
end_c <- 10
GBM_NTREES = 280
GBM_SHRINKAGE = 0.005 
GBM_DEPTH = 20
GBM_MINOBS = 5
############################################################################# 
for ( i in 1:end_c ) {
    GBM_model_1 <- gbm.fit( 
        x = clean.train[1:end_trn,],
        y = target,
        distribution = "gaussian",
        n.trees = GBM_NTREES,
        shrinkage = GBM_SHRINKAGE,
        interaction.depth = GBM_DEPTH,
        n.minobsinnode = GBM_MINOBS,
        bag.fraction = 0.5,
        verbose = TRUE) 
    pr1 <- predict.gbm(object = GBM_model_1,
                       newdata = train[(end_trn+1):end,],
                       GBM_NTREES)
    tr1 <- predict.gbm(object = GBM_model_1,
                       newdata = train[1:end_trn,],
                       GBM_NTREES)
    pr <- pr + pr1
    tr <- tr + tr1
}
pr<-pr/end_c
tr<-tr/end_c
########################################### 
pr1<-round(pr)
tr1<-round(tr)
err(target,tr1)
#################################################### 

write.table(pr1, file = "gbm02.predictions.csv", row.names = FALSE, col.names=FALSE)