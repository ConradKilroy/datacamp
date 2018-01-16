#https://campus.datacamp.com/courses/financial-trading-in-r/

#install.packages("quantmod")

library(quantmod)


mystock <- list(sym = "JWN",
                init = "2017-01-01",
                end = "2017-12-25",
                the_source = "yahoo"
                )


getSymbols(mystock$sym, 
           from = mystock$init, 
           to = mystock$end, 
           src =  mystock$the_source, 
           adjust =  TRUE)

#plot(Cl(JWN))
plot(Cl(
  eval(as.name(mystock$sym))
))

#source package used for 3MonthMoving Avg 3MM
#TTR
lines(SMA(Cl(  
  eval(as.name(mystock$sym))   
), n = 90), col = "red")
