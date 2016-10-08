# Ranking Hospitals By Outcome in A State
# Function: rankhospital
# Description: 
#     Takes three arguments: 
#           - 2-character abbreviated name of a state (state),
#           - an outcome (outcome),
#           - ranking of a hospital in that state for that outcome (num): "best", "worst", or an integer indicating the ranking (smaller numbers are better)
# Data Input: outcome-of-care-measures.csv 
# Return Value: Character vector with the name of the hospital that has the ranking speci???ed by the num argument. 
# Example: 
#     - rankhospital("MD", "heart failure", 5)
#     - returns character vector containing name of hospital with the 5th lowest 30-day death rate for heart failure
# Corner Cases:
#   - If num > # hospitals instate:
#         - then the function should return NA. 
#   - If hospitals does not have data on a particular outcome:
#         - should be excluded from the set of hospitals when deciding the rankings.
#   - If there is a tie:
#         - if multiple hospitals have same 30-day mortality rate for given cause of death. 
#           THe ties should be broken by using the hospital name. 
#         - Eg: In Texas ("TX"), the hospitals with lowest 30-day mortality rate for heart failure are shown here.
#             > head(texas)
#               Out:
#                      Hospital.Name                    Rate  Rank 
#                 3935 FORT DUNCAN MEDICAL CENTER       8.1   1 
#                 4085 TOMBALL REGIONAL MEDICAL CENTER  8.5   2 
#                 4103 CYPRESS FAIRBANKS MEDICAL CENTER 8.7   3 
#                 3954 DETAR HOSPITAL NAVARRO           8.7   4 
#                 4010 METHODIST HOSPITAL,THE           8.8   5 
#                 3962 MISSION REGIONAL MEDICAL CENTER  8.8   6
#           - Note that Cypress Fairbanks Medical Center and Detar Hospital Navarro both have the same 30-day rate (8.7). 
#             However, because Cypress comes before Detar alphabetically, Cypress is ranked number 3 in this scheme and 
#             Detar is ranked number 4. One can use the order function to sort multiple vectors in this manner 
#             (i.e. where one vector is used to break ties in another vector).
#     - Invalid Arguments:
#           - invalid state: function should throw an error via the stop function with the exact message "invalid state"
#           - invalid outcome: function should throw an error via the stop function with the exact message "invalid outcome".
#             

rankhospital <- function(state, outcome, num = "best") {
  ##setwd()
  setwd("C:/Users/Rikesh/Learning/DataScience/CourseraDataScienceSpecialization/1DataScienceR/week4project/")
  ## Read outcome data
  outcomeData <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  ## Check that state and outcome are valid
  statesList<-unique(outcomeData$State)
  try(if(!(state%in%statesList))stop("invalid state"))
  possibleOutcomes <- c("heart attack","heart failure","pneumonia")
  try(if(!(outcome%in%possibleOutcomes))stop("invalid outcome"))
  
  ## Format outcome input arg
  formatOutcome<-simpleCap(outcome)
  formatOutcome<-paste("Hospital.30.Day.Death..Mortality..Rates.from", formatOutcome,sep=".")
  
  ## Return hospital name in that state with the given rank ## 30-day death rate
  newDf <- subset(outcomeData[outcomeData$State == state , ])
  newDf <- data.frame(newDf, stringsAsFactors = FALSE)
  newDf <- newDf[newDf[formatOutcome]!='Not Available', ]
  newDf <- newDf[with(newDf, order(as.numeric(unlist(newDf[formatOutcome])), Hospital.Name)), ]
  newDf <- newDf[!(is.na(newDf[formatOutcome]) | newDf[formatOutcome]==""), ]
  
  if(num == "best"){
    resultsdf <- head(newDf, 1)
    }
  else if(num == "worst"){
    resultsdf <- tail(newDf, 1)
  }
  else{
    if(num >length(newDf)){
      print("NA")
      return
    }
    resultsdf <- newDf[num, ]
  }
  resultsdf$Hospital.Name
}

simpleCap <- function(x) {
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1,1)), substring(s, 2),
        sep="", collapse=".")
}
