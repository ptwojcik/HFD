positionVB <- function(signal, 
                       lower, 
                       upper, 
                       times_data,
                       time_lower = "9:40:00",
                       time_upper = "15:50:00",
                       strategy)
{
  
  require(xts)
  
  # lets check thevalue of the strategy parameter
  if (! strategy %in% c("mom", "mr"))
  {  print("Strategy parameter incorrect. Please use 'mom' or 'mr'!")
     stop
  }
  
  signal <- coredata(signal)
  lower <- coredata(lower)
  upper <- coredata(upper)
  
  # lets first create a vector of 0s
  position <- rep(0, length(signal))
  
  for (i in 2:length(signal))
  {
    if ( times(times_data[i] ) <= times(time_lower) | 
       times(times_data[i] ) > times(time_upper)
     ) { position[i] <- 0 }
    else
    { # check if values are nonmissing (otherwise calculations not possible)
      if (!is.na(signal[i-1]) & 
            !is.na(upper[i-1]) & 
            !is.na(lower[i-1]))
      { 
        # what if previous position was 0
        if (position[i-1] == 0){
          if (signal[i-1] > upper[i-1]){position[i] <- -1}
          if (signal[i-1] < lower[i-1]){position[i] <- 1}
        } else if (position[i-1]==-1){
          # what if previous position was -1
          if (signal[i-1] > lower[i-1]){position[i] <- -1}
          if (signal[i-1] < lower[i-1]){position[i] <- 1}
        } else if (position[i-1]==1){
          # what if previous position was 1
          if (signal[i-1] < upper[i-1]){position[i] <- 1}
          if (signal[i-1] > upper[i-1]){position[i] <- -1}
        }
      } else position[i] <- position[i-1]
      # if anything is missing, keep previous position
    }
  }
  # reverse the position if we use a momentum ("mom") strategy
  if(strategy == "mom") position <- (-position)
  
  # return() function clearly indicates 
  # what the function should return
  return(position)
}
