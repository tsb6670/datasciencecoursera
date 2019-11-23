## Question 3 (Ranking hospital by outcome in a state)
## Takes three arguments: the 2-character abbreviated name of a state (state), 
## an outcome (outcome), and the ranking of a hospital in that state for that 
## outcome (num). The function reads the outcome-of-care-measures.csv file and 
## returns a character vector with the name of the hospital that has the ranking 
## specified by the num argument.

rankhospital <- function(state, outcome, num = "best") {
  ## Read outcome data
  data <- read.csv("course2_wk4_data/outcome-of-care-measures.csv", colClasses = "character")
  
  ## Check that state and outcome are valid
  if(! state %in% data$State)
    stop("invalid state")
  
  ## If the number given by num is larger than the number 
  ## of hospitals in that state, then the function should return NA.
  options(warn=-1)
  vl <- data.frame()
  reqRanking <- numeric()
  if(outcome == "heart attack")
  {
    data[, 11] <- as.numeric(data[, 11])
    data.state <- data[data$State == state,]
    data.clean <- na.omit(data.state)
    
    data.clean$ranked <- NA
    data.prep   <- subset(data.clean, select=c(Hospital.Name, Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack, ranked))
    data.order  <- data.prep[order(rank(data.prep$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack)),]
    data.ranked <- transform(data.order, ranked = rank(Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack))
    
    names(data.ranked)[1]<-paste("Hospital.Name")
    names(data.ranked)[2]<-paste("Rate")
    names(data.ranked)[3]<-paste("Actual.Rank")
    
    data.staged <- data.ranked[order(data.ranked$Actual.Rank, data.ranked$Hospital.Name),]
    data.staged$rank <- 1:nrow(data.staged)
    names(data.staged)[4]<-paste("Rank")
    data.done  <- subset(data.staged, select=c(Hospital.Name, Rate, Rank))
    vl <- data.done
    
    # set the desired ranking
    if(class(num) == "character" && num == "best")
    {
      reqRanking <- as.integer(1)
    } else if(class(num) == "character" && num == "worst")
    {
      reqRanking <- as.integer(max(vl$Rank))
    } else if(class(as.numeric(num)) == "numeric")
    {
      reqRanking <- as.numeric(num)
    } else
    {
      stop("invalid state")
    }
    
  } else if(outcome == "heart failure")
  {
    data[, 17] <- as.numeric(data[, 17])
    data.state <- data[data$State == state,]
    data.clean <- na.omit(data.state)
    
    data.clean$ranked <- NA
    data.prep   <- subset(data.clean, select=c(Hospital.Name, Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure, ranked))
    data.order  <- data.prep[order(rank(data.prep$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure)),]
    data.ranked <- transform(data.order, ranked = rank(Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure))
    
    names(data.ranked)[1]<-paste("Hospital.Name")
    names(data.ranked)[2]<-paste("Rate")
    names(data.ranked)[3]<-paste("Actual.Rank")
    
    data.staged <- data.ranked[order(data.ranked$Actual.Rank, data.ranked$Hospital.Name),]
    data.staged$rank <- 1:nrow(data.staged)
    names(data.staged)[4]<-paste("Rank")
    data.done  <- subset(data.staged, select=c(Hospital.Name, Rate, Rank))
    vl <- data.done
    
    # set the desired ranking
    if(class(num) == "character" && num == "best")
    {
      reqRanking <- as.integer(1)
    } else if(class(num) == "character" && num == "worst")
    {
      reqRanking <- as.integer(max(vl$Rank))
    } else if(class(as.numeric(num)) == "numeric")
    {
      reqRanking <- as.numeric(num)
    } else
    {
      stop("invalid state")
    }
  }
  else if(outcome == "pneumonia")
  {
    data[, 23] <- as.numeric(data[, 23])
    data.state <- data[data$State == state,]
    data.clean <- na.omit(data.state)
    
    data.clean$ranked <- NA
    data.prep   <- subset(data.clean, select=c(Hospital.Name, Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia, ranked))
    data.order  <- data.prep[order(rank(data.prep$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia)),]
    data.ranked <- transform(data.order, ranked = rank(Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia))
    
    names(data.ranked)[1]<-paste("Hospital.Name")
    names(data.ranked)[2]<-paste("Rate")
    names(data.ranked)[3]<-paste("Actual.Rank")
    
    data.staged <- data.ranked[order(data.ranked$Actual.Rank, data.ranked$Hospital.Name),]
    data.staged$rank <- 1:nrow(data.staged)
    names(data.staged)[4]<-paste("Rank")
    data.done  <- subset(data.staged, select=c(Hospital.Name, Rate, Rank))
    vl <- data.done
    
    # set the desired ranking
    if(class(num) == "character" && num == "best")
    {
      reqRanking <- as.integer(1)
    } else if(class(num) == "character" && num == "worst")
    {
      reqRanking <- as.integer(max(vl$Rank))
    } else if(class(as.numeric(num)) == "numeric")
    {
      reqRanking <- as.numeric(num)
    } else
    {
      stop("invalid state")
    }
  }
  
  ## Return hospital name in that state with the given rank
  ## 30-day death rate
  if(reqRanking > max(vl$Rank))
    return(NA)
  else
    rt <- vl[vl$Rank == reqRanking,]
  
  # return vector
  x <- rt["Hospital.Name"]
  x[1,]
}
