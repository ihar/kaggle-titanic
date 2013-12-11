# Run this script if you want to generate all submission files.
rm(list=ls(all=TRUE))

source("load.R")
source("functions-clean.R")
source("clean.R")

rm(list=ls(all=TRUE))

source("functions-prediction.R")
source("logit.R")
source("rf.R")
source("svm.R")
source("gbm.R")







