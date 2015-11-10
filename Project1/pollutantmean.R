pollutantmean <- function(directory, pollutant, id = 1:332) {
	## 'directory' is a character vector of length 1 indicating
	## the location of the CSV files

	## 'pollutant' is a character vector of length 1 indicating
	## the name of the pollutant for which we will calculate the
	## mean; either "sulfate" or "nitrate".

	## 'id' is an integer vector indicating the monitor ID numbers
	## to be used

	## Return the mean of the pollutant across all monitors list
	## in the 'id' vector (ignoring NA values)
	## NOTE: Do not round the result!
  thispollutant<-pollutant
  firstOne <- TRUE
	for(i in id){
   fname <-paste("00",i,sep="")
	  fname <-substring(fname,nchar(fname)-2)
	  thisfpath <- file.path(directory,paste(fname,".csv",sep=""))
	  thisfexists <- file.exists(thisfpath)
	  thisfile <- read.csv(thisfpath)
	  thisresults <- thisfile[[thispollutant]][!is.na(thisfile[[thispollutant]])]
	  if (firstOne) {
	    results <- thisresults
	    firstOne <- FALSE
	  } else {
	    results <- c(results,thisresults)
	  }
	}
  mean(results)
}

