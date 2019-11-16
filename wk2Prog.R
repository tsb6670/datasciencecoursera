# calculates the mean of a pollutant (sulfate or nitrate) across 
# a specified list of monitors. The function 'pollutantmean' takes 
# three arguments: 'directory', 'pollutant', and 'id'.

pollutantmean <- function(directory, pollutant, id = 1:332) {
   ## 'directory' is a character vector of length 1 indicating
   ## the location of the CSV files
   
   ## 'pollutant' is a character vector of length 1 indicating
   ## the name of the pollutant for which we will calculate the
   ## mean; either "sulfate" or "nitrate"
   
   ## 'id' is an integer vector indicating the monitor ID numbers
   ## to be used
   
   fn_id = id
   
   ## Return the mean of the pollutant across all monitors list
   ## in the 'id' vector (ignoring NA values)
   ## NOTE: Do not round the result!
   
   # iterate over the monitor files and load the CSV in a single
   # data frame called 'data'
   
   # "Date","sulfate","nitrate","ID"
   data <- data.frame(Date=as.Date(character()), sulfate=numeric(), nitrate=numeric(), ID=integer())
   
   for(i in seq_along(fn_id)) {
      fn <- ""
      if(fn_id[i] < 10) { 
         fn <- paste(directory,"/", "00", as.character(fn_id[i]), ".csv", sep = '')
      } else if(fn_id[i] < 100) { 
         fn <- paste(directory,"/","0", as.character(fn_id[i]),  ".csv", sep = '') 
      } else {
         fn <- paste(directory,"/", as.character(fn_id[i]),  ".csv", sep = '')
      }
      
      fn_data <- read.csv(fn)
      data <- rbind(data, fn_data)
   }
   
   mean(data[[pollutant]], na.rm = TRUE)
}


# Read a directory full of files and report the number of completely observed cases in each file.
# The function should return a data frame where the first column is the name of the file and the
# second column is the number of complete cases.

complete <- function(directory, id = 1:332) {
   ## 'directory' is a character vector of length 1 indicating
   ## the location of the CSV files
   
   ## 'id' is an integer vector indicating the monitor ID numbers
   ## to be used
   fn_id = id
   
   ## Return a data frame of the form:
   ## id nobs
   ## 1  117
   ## 2  1041
   ## ...
   ## where 'id' is the monitor ID number and 'nobs' is the
   ## number of complete cases
   data <- data.frame(id=as.integer(), nobs=integer())
   
   for(i in seq_along(fn_id)) {
      fn <- ""
      if(fn_id[i] < 10) { 
         fn <- paste(directory,"/", "00", as.character(fn_id[i]), ".csv", sep = '')
      } else if(fn_id[i] < 100) { 
         fn <- paste(directory,"/","0", as.character(fn_id[i]),  ".csv", sep = '') 
      } else {
         fn <- paste(directory,"/", as.character(fn_id[i]),  ".csv", sep = '')
      }
      
      fn_data <- read.csv(fn)
      newdata <- fn_data[complete.cases(fn_data), ]
      
      df <- data.frame(fn_id[i], nrow(newdata))
      names(df) <- c("id", "nobs")
      
      data <- rbind(data, df)
   }
   
   data
}


# take a directory of data files and a threshold for complete cases and calculate the correclation
# between sulfate and nitrate for monitor locations where the number of completely observed cases
# (on all variables) is greater than the threshold. The function should return a vector of
# correlations for the monitors that meet the threshold requirement. If no monitors meet the
# threshold requirement, then the function should return a numeric vector of length 0.

corr <- function(directory, threshold = 0) {
   # "Date","sulfate","nitrate","ID"
   # data <- data.frame(Date=as.Date(character()), sulfate=numeric(), nitrate=numeric(), ID=integer())

   ## 'directory' is a character vector of length 1 indicating
   ## the location of the CSV files
   
   ## 'threshold' is a numeric vector of length 1 indicating the
   ## number of completely observed observations (on all variables)
   ## required to compute the correlation between nitrate and sulfate;
   ## the default is 0
   
   corVector <- c()
   filelst <- list.files(directory)
   for(i in seq_along(filelst)) {
      fn <- paste(directory, '/', filelst[i], sep='')

      fn_data <- read.csv(fn)
      
      newdata <- fn_data[complete.cases(fn_data), ]
      nobs <- nrow(newdata)
      if(nobs > threshold) {
         vect <-cor(newdata$sulfate, newdata$nitrate)
         corVector <- c(corVector, vect)
      }
   }
   
   ## Return a numeric vector of correlations
   ## NOTE: Do not round the result.
   # if(length(corVector) == 0) { 0 } else { corVector }
   corVector
}




