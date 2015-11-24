source("sortcolumns.R")
source("rankfunctions.R")

#######
# https://class.coursera.org/rprog-034/forum/thread?thread_id=154
#
# Harris County Hospital has a 10.0 mortality rate, Fort Duncan has 8.1 for heart failure.
# Did you convert your column "Hospital 30-Day Death (Mortality) Rates from Heart Failure" from character into numeric? If not, it will pick the "10" first as that is the lowest alphabetically, but not numerically.
#######


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
  outcomeRows <- outcomeData[,col2use] != "Not Available"
  tempdata <- outcomeData[thisState & outcomeRows,c(7,2,col2use)]
  # results_sorted_1_col is just a debugging variable to verify that 2nd column is getting sorted
  results_sorted_1_col <- sort_by_column(tempdata,3)
  print(head(results_sorted_1_col))
  resultsSorted <- sort_by_columns(tempdata,3,2)
  print(head(resultsSorted))
  ## Return hospital name in that state with lowest 30-day death
  ## rate

  # hmmm... I'm not convinced that find_city_rank is working correctly
  #results <- find_city_rank(resultsSorted,3,1)
  results <- resultsSorted[1,2]
  results
}