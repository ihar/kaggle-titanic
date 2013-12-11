# Returns honorific title from full name of a passenger
# Full name pattern: "Hickman, Mr. Stanley George"
get_honorific <- function(full_name) {
  first_name <- strsplit(full_name, ", ", fixed=TRUE)[[1]][2] # Mr. Stanley George
  honorific <- strsplit(first_name, ".", fixed=TRUE)[[1]][1] # Mr
  
  if (honorific %in% c("Capt", 
                       "Col", 
                       "Don", 
                       "Dr", 
                       "Jonkheer",
                       "Major",
                       "Rev",
                       "Sir")) honorific = "Mr"
  
  if (honorific %in% c("Dona", "Lady", "Mme", "the Countess")) honorific = "Mrs"
  
  if (honorific %in% c("Mlle", "Ms")) honorific = "Miss"
  
  
  return(honorific)
}

# Feature scaling
scale_feature <- function(values) {
  scaled_feature <- (values - min(values)) / (max(values - min(values)))
  return(scaled_feature)
}
