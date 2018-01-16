#https://campus.datacamp.com/courses/financial-trading-in-r/

install.packages("quantmod")

library(quantmod)



# Get SPY from yahoo
getSymbols("SPY", 
           from = "2000-01-01", 
           to = "2017-11-22", 
           src =  "google", 
           adjust =  TRUE)

# Plot the closing price of SPY
plot(Cl(SPY))

#plot the high price of SPY
plot(Hi(SPY))


#source package used for 3MonthMoving Avg 3MM
#TTR
lines(SMA(Cl(SPY), n = 90), col = "red")
