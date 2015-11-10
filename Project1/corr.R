source("complete.R")
corr <- function(directory, threshold = 0) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files

  ## 'threshold' is a numeric vector of length 1 indicating the
  ## number of completely observed observations (on all
  ## variables) required to compute the correlation between
  ## nitrate and sulfate; the default is 0

  ## Return a numeric vector of correlations
  ## NOTE: Do not round the result!

  files <- complete(directory)
  completeFiles <- files[files$nobs > threshold,]

  firstOne <- TRUE
  for(i in seq_along(completeFiles$id)){
    thisID = completeFiles$id[i]
#    print (paste("thisID:",thisID))
    fname <-paste("00",thisID,sep="")
    fname <-substring(fname,nchar(fname)-2)
    thisfpath <- file.path(directory,paste(fname,".csv",sep=""))
#    print (paste("thisfpath:",thisfpath))
    thisfile <- read.csv(thisfpath)
    thisCor <- cor(thisfile$sulfate, thisfile$nitrate,use="complete.obs")

    if (firstOne) {
      results <- thisCor
      firstOne <- FALSE
    } else {
      results <- c(results,thisCor)
    }
  }
  if (firstOne) { # we never went through the FOR loop
    results <- numeric(0)
  }
  results

}