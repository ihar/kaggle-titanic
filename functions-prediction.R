# Save predictions to a file
save_prediction <- function(fname, prediction) {
  res.df <- data.frame(PassengerId = 892:1309, Survived = prediction)
  write.table(res.df, 
              file=paste0("submissions", "/", fname),
              row.names=FALSE, 
              sep=",",
              quote = FALSE)
}