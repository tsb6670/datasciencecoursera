## Question 4 (Ranking hospitals in all states)
## that takes two arguments: an outcome name (outcome) and a hospital ranking (num). 
## The function reads the outcome-of-care-measures.csv file and returns a 2-column 
## data frame containing the hospital in each state that has the ranking specified 
## in num. 

rankall <- function(outcome, num = "best") {
  
  ## Scoping function variables
  vl <- data.frame(Hospital.Name=character(), State=character(), Rate=numeric(), Actual.Rank=numeric(), Rank=integer())
  reqRanking <- numeric()
  
  ## Read outcome data
  data <- read.csv("course2_wk4_data/outcome-of-care-measures.csv", colClasses = "character")
  
  options(warn=-1)
  data[, 11] <- as.numeric(data[, 11])
  data[, 17] <- as.numeric(data[, 17])
  data[, 23] <- as.numeric(data[, 23])
  
  # Create a set of factors (states)
  states.unique <- sort(unique(data$State))
  
  ## Check that outcome is valid
  if(outcome == "heart attack")
  {
    for(state in states.unique)
    {
      data.state <- data[data$State == state,]
      data.clean <- na.omit(data.state)
      
      data.clean$ranked <- NA
      data.prep   <- subset(data.clean, select=c(Hospital.Name, State, Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack, ranked))
      data.order  <- data.prep[order(rank(data.prep$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack)),]
      data.ranked <- transform(data.order, ranked = rank(Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack))
      
      names(data.ranked)[1]<-paste("Hospital.Name")
      names(data.ranked)[2]<-paste("State")
      names(data.ranked)[3]<-paste("Rate")
      names(data.ranked)[4]<-paste("Actual.Rank")
      
      data.staged <- data.ranked[order(data.ranked$Actual.Rank, data.ranked$Hospital.Name),]
      data.staged$rank <- 1:nrow(data.staged)
      names(data.staged)[5]<-paste("Rank")
      
      data.done  <- subset(data.staged, select=c(Hospital.Name, State, Rate, Actual.Rank, Rank))
      vl <- rbind(vl, data.done)
    }
    
  } else if(outcome == "heart failure")
  {
    
    for(state in states.unique)
    {
      data.state <- data[data$State == state,]
      data.clean <- na.omit(data.state)
      
      data.clean$ranked <- NA
      data.prep   <- subset(data.clean, select=c(Hospital.Name, State, Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure, ranked))
      data.order  <- data.prep[order(rank(data.prep$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure)),]
      data.ranked <- transform(data.order, ranked = rank(Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure))
      
      names(data.ranked)[1]<-paste("Hospital.Name")
      names(data.ranked)[2]<-paste("State")
      names(data.ranked)[3]<-paste("Rate")
      names(data.ranked)[4]<-paste("Actual.Rank")
      
      data.staged <- data.ranked[order(data.ranked$Actual.Rank, data.ranked$Hospital.Name),]
      data.staged$rank <- 1:nrow(data.staged)
      names(data.staged)[5]<-paste("Rank")
      
      data.done  <- subset(data.staged, select=c(Hospital.Name, State, Rate, Actual.Rank, Rank))
      vl <- rbind(vl, data.done)
    }
    
  } else if(outcome == "pneumonia")
  {
    for(state in states.unique)
    {
      data.state <- data[data$State == state,]
      data.clean <- na.omit(data.state)
      
      data.clean$ranked <- NA
      data.prep   <- subset(data.clean, select=c(Hospital.Name, State, Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia, ranked))
      data.order  <- data.prep[order(rank(data.prep$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia)),]
      data.ranked <- transform(data.order, ranked = rank(Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia))
      
      names(data.ranked)[1]<-paste("Hospital.Name")
      names(data.ranked)[2]<-paste("State")
      names(data.ranked)[3]<-paste("Rate")
      names(data.ranked)[4]<-paste("Actual.Rank")
      
      data.staged <- data.ranked[order(data.ranked$Actual.Rank, data.ranked$Hospital.Name),]
      data.staged$rank <- 1:nrow(data.staged)
      names(data.staged)[5]<-paste("Rank")
      
      data.done  <- subset(data.staged, select=c(Hospital.Name, State, Rate, Actual.Rank, Rank))
      vl <- rbind(vl, data.done)
    }
    
  } else { stop("invalid state") }
  
  ## For each state, find the hospital of the given rank
  # set the desired ranking 
  
  # rt.filtered <- vl[vl$Rank == reqRanking,]
  # rt.done     <- subset(rt.filtered, select=c(Hospital.Name, State))
  
  dt <- data.frame(Hospital.Name=character(), State=character())
  for(state in states.unique)
  {
    rt.staged <- vl[vl$State == state,]
    
    if(class(num) == "character" && num == "best")
    {
      reqRanking <- as.integer(min(rt.staged$Rank))
    } else if(class(num) == "character" && num == "worst")
    {
      reqRanking <- as.integer(max(rt.staged$Rank))
    } else if(class(as.numeric(num)) == "numeric")
    {
      reqRanking <- as.numeric(num)
    } else
    {
      stop("invalid state")
    }
    
    rt.filtered <- rt.staged[rt.staged$Rank == reqRanking,]
    rt.done     <- subset(rt.filtered, select=c(Hospital.Name, State))
    
    if(! nrow(rt.done[rt.done$State == state,]) > 0)
    {
      tmp <- data.frame(NA, state)
      names(tmp)<-c("Hospital.Name", "State")
      row.names(tmp) <- state
      
      dt <- rbind(dt, tmp)
    }
    else
    {
      nw <- data.frame(rt.done[rt.done$State == state,])
      names(nw)<-c("Hospital.Name", "State")
      row.names(nw) <- state
      
      dt <- rbind(dt, nw)
    }
  }
  
  # rt <- rt.done[order(rt.done[,2], rt.done[,1]), ]
  rt <- dt[order(dt[,2], dt[,1]), ]
  
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
  
  if(reqRanking > max(vl$Rank))
    return(NA)
  
  ## Return a data frame with the hospital names and the
  ## (abbreviated) state name
  rt
}
