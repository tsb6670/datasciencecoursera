## Question #2 (Finding the best hospital in a state) 
## best takes two arguments: the 2-character abbreviated name of a state and an
## outcome name. The function reads the outcome-of-care-measures.csv file and 
## returns a character vector with the name of the hospital that has the best 
## (i.e. lowest) 30-day mortality for the specified outcome in that state. The 
## hospital name is the name provided in the Hospital.Name variable. The outcomes 
## can be one of “heart attack”, “heart failure”, or “pneumonia”. Hospitals that 
## do not have data on a particular outcome should be excluded from the set of 
## hospitals when deciding the rankings.

best <- function(state, outcome) {
  ## Read outcome data
  data <- read.csv("course2_wk4_data/outcome-of-care-measures.csv", colClasses = "character")
  
  ## Check that state and outcome are valid
  if(! state %in% data$State)
    stop("invalid state")
  else if(! outcome %in% c("heart attack", "heart failure", "pneumonia"))
    stop("invalid state")
  
  ## Return hospital name in that state with lowest 30-day death
  ## rate
  options(warn=-1)
  vl <- data.frame()
  if(outcome == "heart attack")
  {
    data[, 11] <- as.numeric(data[, 11])
    dt <- tapply(data$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack, data$State, min, na.rm=T)
    vl <- subset(data, Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack == dt[state] & State == state, select=Hospital.Name)
    
  } else if(outcome == "heart failure")
  {
    data[, 17] <- as.numeric(data[, 17])
    dt <- tapply(data$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure, data$State, min, na.rm=T)
    vl <- subset(data, Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure == dt[state] & State == state, select=Hospital.Name)
  }
  else if(outcome == "pneumonia")
  {
    data[, 23] <- as.numeric(data[, 23])
    dt <- tapply(data$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia, data$State, min, na.rm=T)
    vl <- subset(data, Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia == dt[state] & State == state, select=Hospital.Name)
  }
  
  # sort alphabetically
  vl <- vl[order(vl[,1])]
  
  # return vector
  vl[1,]
}
