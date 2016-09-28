# Script Function: Finding the best hospital in a state
# Function name: best
# Specifications:
#     - arguments:
#         1) the 2-character abbreviated name of a state
#         2) outcome name
# Data Source: outcome-of-care-measures.csv
# Return:
#     - returns character vector with name of the hospital that has the best
#       (i.e. lowest) 30-day mortality for the specified outcome in that state.
#       The hospital name is the name provided in the Hospital.Name variable.
#       + The outcomes can be one of:
#         -"heart attack"
#         -"heart failure"
#         -"pneumonia"
# Corner Cases:
#     - Hospitals that do not have data on a particular outcome should be excluded
#       from the set of hospitals when deciding the rankings
#     - Handling ties: If there is a tie for the best hospital for a given outcome,
#       then the hospital names should be stored in alphabetical order and the first
#       hospital in that set should be chosen (i.e. if hospitals "b", "c", and "f"
#       are tied for best, then hospital "b" should be returned)

#Notes: We are looking at column 11 and 19
best <- function(state, outcome) { 
  ##setwd()
  setwd("C:/Users/Rikesh/Learning/DataScience/CourseraDataScienceSpecialization/1DataScienceR/week4project/")
  ## Read outcome data
  outcomeData <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  ## Check that state and outcome are valid
  statesList<-unique(outcomeData$State)
  try(if(!(state%in%statesList))stop("invalid state"))
  possibleOutcomes <- c("heart attack","heart failure","pneumonia")
  try(if(!(outcome%in%possibleOutcomes))stop("invalid outcome"))
  newDf <- subset(outcomeData[outcomeData$State == state , ])
  ## Return hospital name in that state with lowest 30-day death rate
  formatOutcome<-simpleCap(outcome)
  formatOutcome<-paste("Hospital.30.Day.Death..Mortality..Rates.from", formatOutcome,sep=".")
  #remove all the NA valued data
  #newDf<- complete.cases(newDf[, formatOutcome])
  #find min value for mortality
  data<- min(na.omit(as.numeric(unlist(newDf[formatOutcome]))))
  results <-subset(newDf, as.numeric(unlist(newDf[formatOutcome])) == data)
  hospitalNames <-sort(results$Hospital.Name)
  hospitalNames
}

simpleCap <- function(x) {
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1,1)), substring(s, 2),
        sep="", collapse=".")
}