best <- function(state, outcome) {
  ## Read outcome data
  outcomeData <- read.csv("outcome-of-care-measures.csv", colClasses = "character")

  ## Check that state and outcome are valid
  thisState <-outcomeData$State == state
  validState <- nrow(outcomeData[thisState,])
  if (!validState) {
    stop("invalid state")
    return (paste(state,"is not a valid state"))
  }
  if ((outcome != "heart attack") && (outcome != "heart failure") && (outcome != "pneumonia")) {
    stop("invalid outcome")    
  }
  # if we get to this point then we've got a valid outcome, now to see which one...
  # default to "heart attack"
  col2use <- 11
  if (outcome == "heart failure") {col2use <- 17}
  if (outcome == "pneumonia") {col2use <- 23}
  # $State = 7
  # $Hospital.Name = 2
  outcomeRows <- outcome[[col2use]] != "Not Available"
  results <- outcomeData[thisState && outcomeRows,c(7,2,col2use)]
#  results <- outcomeData[thisState,c(7,2,col2use)]
  results
  #print (paste("Counting for", state))
  ## Return hospital name in that state with lowest 30-day death
  ## rate
}