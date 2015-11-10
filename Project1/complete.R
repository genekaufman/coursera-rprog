complete <- function(directory, id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## Return a data frame of the form:
  ## id nobs
  ## 1  117
  ## 2  1041
  ## ...
  ## where 'id' is the monitor ID number and 'nobs' is the
  ## number of complete cases
  ids<-numeric(length(id))
  nobs<-numeric(length(id))
	for(i in seq_along(id)){
	  thisID = id[i]
    fname <-paste("00",thisID,sep="")
	  fname <-substring(fname,nchar(fname)-2)
	  thisfpath <- file.path(directory,paste(fname,".csv",sep=""))
	  thisfile <- read.csv(thisfpath)
	  thisfilecomplete <- sum(complete.cases(thisfile))
    thisresults <- c("id"=i,"nobs"=i+i)
    ids[i]<-thisID
    nobs[i]<-thisfilecomplete
	}
  data.frame("id"=ids,nobs)
}


