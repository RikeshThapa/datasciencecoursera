pollutantmean<-function(directory, pollutant, id=1:322){
  files<-sprintf("%03d.csv", id)
  pollutiondf <- data.frame()
  for (f in files){
    tempdata <- read.csv(paste(directory,f, sep=""))
    pollutiondf<-rbind(pollutiondf, tempdata)
  }
  colMeans(pollutiondf[pollutant], na.rm = TRUE)
}