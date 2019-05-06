#source: https://www.analyticsvidhya.com/blog/2016/06/bayesian-statistics-beginners-simple-english/

install.packages(stats)
library(stats)
par(mfrow=c(3,2))
x=seq(0,1,by=0.1)
alpha=c(0,2,10,20,50,500)
beta=c(0,2,8,11,27,232)
for(i in 1:length(alpha)){
  y<-dbeta(x,shape1=alpha[i],shape2=beta[i])
  plot(x,y,type="l")
}
