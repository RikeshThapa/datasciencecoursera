corr<-function(directory, threshold=0){
  files <-list.files(path = directory, pattern=".csv", full.names = T, recursive = FALSE)
  completedf <- complete(directory)
  subsetdf <- subset(completedf, completedf$nobs>threshold)
  corrilations <- numeric(0)
  x <- 1
  for (i in subsetdf$id){
    tempdata <- read.csv(files[i])
    corrval <- cor(tempdata$sulfate, tempdata$nitrate, use = "complete.obs")
    corrilations[x]<-corrval
    x<-x+1
  }
  corrilations
}