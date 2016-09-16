complete<-function(directory, id=1:322){
  files<-sprintf("%03d.csv", id)
  completedf <- data.frame()
  count = 1
  for (f in files){
    tempdata <- read.csv(paste(directory,f, sep=""))
    insertrow <- data.frame(id[count], nobs=nrow(na.omit(tempdata)))
    completedf<-rbind(completedf, insertrow)
    count = count+1
  }
  completedf
}