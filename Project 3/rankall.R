source("sortcolumns.R")
source("rankfunctions.R")

rankall <- function(outcome, num = "best") {
  ## Read outcome data
  ## Check that state and outcome are valid
  ## For each state, find the hospital of the given rank
  ## Return a data frame with the hospital names and the
  ## (abbreviated) state name
  ## Read outcome data
  if ((outcome != "heart attack") && (outcome != "heart failure") && (outcome != "pneumonia")) {
    stop("invalid outcome")
  }
  # default to "heart attack"
  col2use <- 11
  if (outcome == "heart failure") {col2use <- 17}
  if (outcome == "pneumonia") {col2use <- 23}
  
  outcomeData <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  outcomeData[,7] <- as.factor(outcomeData[,7])
  outcomeData[,col2use] <- suppressWarnings(as.numeric(outcomeData[,col2use]))
  stateList <- levels(outcomeData[,7])
  
  output <- vector()
  for (i in 1:length(stateList)) {
    ## statedata subsets data by the considered state
    statedata <- outcomeData [grep(stateList[i],outcomeData$State),]
    orderdata <- statedata[order(statedata[,col2use],statedata[,2],decreasing=FALSE,na.last = NA),]
    if (FALSE && i == 2) {
      print(head(statedata[,c(2,7,col2use)]))
      print(head(orderdata[,c(2,7,col2use)]))
      print(paste("statedata:",nrow(statedata),"orderdata:",nrow(orderdata)))
    }
    
    if (is.numeric(num)) {
      rank <- num
    } else if (num == "best") {
      rank <- 1
    } else if (num == "worst") {
      rank <- nrow(orderdata)
    } else {
      stop("requested rank not numeric")
    }
   # if (rank > nrow(orderdata)) {rank <- nrow(orderdata)+1}
  #  print(paste("State:",i,"Rank:",rank,"nrow:",nrow(orderdata)))
    
    # $State = 7
    # $Hospital.Name = 2
    ## append() adds elements at the end of a vector. We want to add the name of the city [rank,1],
    ## the areakm2 [rank,2] and the populationk [rank,3]. We don't add the name of the countries, because it
    ## will be the label of the rows.
    if (rank > nrow(orderdata)) {
      output <- append (output, NA)
    } else {
      output <- append (output, as.character(orderdata[rank,2]))
    }
    output <- append (output, as.character(stateList[i]))
#    output <- append (output, as.character(orderdata[rank,col2use]))
    

  }
  
  ## Just because it's simpler to generate a matrix rather than a data frame, I generate it first and convert it
  ## to data frame immediatly after. 
  output <- as.data.frame(matrix(output,length(stateList),2, byrow = TRUE))
  ## Name of the columns will be "cities", "areakm2" and "populationk". Name of the rows are the countries.
  colnames(output) <- c("hospital","state")
  rownames(output) <- stateList
  return(output)
  
  print(stateList)
  stop("derp")
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

  # $State = 7
  # $Hospital.Name = 2
  outcomeRows <- outcomeData[,col2use] != "Not Available"
  tempdata <- outcomeData[thisState & outcomeRows,c(7,2,col2use)]
  tempdata[,3] <- as.numeric(tempdata[,3])
  # results_sorted_1_col is just a debugging variable to verify that 2nd column is getting sorted
  # results_sorted_1_col <- sort_by_column(tempdata,3)
  # print(head(results_sorted_1_col))
  resultsSorted <- sort_by_columns(tempdata,3,2)
  # print(head(resultsSorted))
  ## Return hospital name in that state with lowest 30-day death
  ## rate
  
  # hmmm... I'm not convinced that find_city_rank is working correctly
  #results <- find_city_rank(resultsSorted,3,1)

  
  results <- resultsSorted[rank,2]
  results
  
}