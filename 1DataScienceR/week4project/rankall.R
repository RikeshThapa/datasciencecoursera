# Function Name: rankall.R
# Arguments: outcome (outcome name), num (hospital ranking)
#           - num variable can take values "best", "worst", or an integer indicating the ranking
# Data Source: outcome-of-care-measure.csv
# Purpose: returns a 2-column data frame containing the hospital in each state that has the ranking speci???ed in num. 
# Example: rankall("heart attack", "best") 
#         > returns data frame containing the names of the hospitals that are the best 
#           in their respective states for 30-day heart attack death rates. 
         
# return: - The function should return a value for every state (some may be NA). 
#         - 1st column: 
#             - named hospital, which contains the hospital name, 
#         - 2nd column:
#             - "state": the 2-character abbreviation for the state name. 
# Corer Cases:- Hospitals w/ data on a particular outcome should be excluded from the set of hospitals 
#             when deciding the rankings.
#             - Handling ties: 
#               - in the 30-day mortality rates in the same way that the rankhospital function handles ties.
#             - Invalid args: throw an error via the stop function with the exact message "invalid outcome"
#             - Number given by num is larger than the number of hospitals in that state: function should return NA
require(data.table)

rankall <- function(outcome, num = "best") { 
  ##setwd()
  setwd("C:/Users/Rikesh/Learning/DataScience/CourseraDataScienceSpecialization/1DataScienceR/week4project/")
  ## Read outcome data
  outcomeData <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  ## Check that state and outcome are valid
  possibleOutcomes <- c("heart attack","heart failure","pneumonia")
  try(if(!(outcome%in%possibleOutcomes))stop("invalid outcome"))
  ## Format outcome input arg
  formatOutcome<-simpleCap(outcome)
  formatOutcome<-paste("Hospital.30.Day.Death..Mortality..Rates.from", formatOutcome,sep=".")
  newDf <- data.table(outcomeData[formatOutcome], key= 'State')
  if(num=='best'){
    newDf <- newDf[,.SD[which.min(formatOutcome)],by=State]
  }
  else if(num=='worst'){
    newDf <- newDf[,.SD[which.max(formatOutcome)],by=State]
  }
  else{
    newDf <- newDf[,head(.SD[(formatOutcome)], num), by=State]
  }
  
  newDf
}

simpleCap <- function(x) {
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1,1)), substring(s, 2),
        sep="", collapse=".")
}