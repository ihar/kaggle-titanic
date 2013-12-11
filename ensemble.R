# Ensembles
source("functions-prediction.R")
# Select the best results for each classificator 
logit_submission <- read.csv("submissions//logit1.csv") # 0.78469 in the Leaderboard
rf_submission <- read.csv("submissions//rf3.csv") # 0.81340
svm_submission <- read.csv("submissions//svm1.csv") # 0.79426
gbm_submission <- read.csv("submissions//gbm1.csv") # 0.78947

# Define final response using majority voting
all_submissions <- cbind(logit_submission$Survived,
                         rf_submission$Survived,
                         svm_submission$Survived,
                         gbm_submission$Survived)
major_voices <- apply(all_submissions, 1, median)
# If there is 0.5 in the result, set it to the value from best submission (rf_submission) 
major_voices[major_voices == 0.5] <- rf_submission$Survived[major_voices == 0.5]
save_prediction("ensemble1.csv", major_voices) # 0.81340

# If there is 0.5 in the result, set it to the value from gbm submission
major_voices <- apply(all_submissions, 1, median)
major_voices[major_voices == 0.5] <- gbm_submission$Survived[major_voices == 0.5]
save_prediction("ensemble2.csv", major_voices) # 0.80383


# Exclude logit submission and using majority again
all_submissions2 <- cbind(rf_submission$Survived,
                         svm_submission$Survived,
                         gbm_submission$Survived)
major_voices2 <- apply(all_submissions2, 1, median)
save_prediction("ensemble3.csv", major_voices2) # 0.80383
