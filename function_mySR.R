# Sharpe ratio
mySR <- function(x, # x = series of returns
                 scale) # scale parameter = Nt
{
  sqrt(scale) * mean(coredata(x), na.rm = T) / 
                sd(coredata(x), na.rm = T)
} # end of definition
