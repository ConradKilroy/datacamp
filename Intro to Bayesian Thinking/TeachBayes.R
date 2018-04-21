#datacamp Bayes in R
#https://campus.datacamp.com/courses/beginning-bayes-in-r/introduction-to-bayesian-thinking

install.packages("TeachBayes")

library(TeachBayes)

areas <- c(2,1,2,1,2)
spinner_plot(areas)

(df <- data.frame(Region = 1:5, 
                  areas,
                  Probability = areas / sum(areas))) 

#probablity of odd spin
library(dplyr)
filter(df, Region %in% c(1,3,5))

#probablity of spins >3
filter(df, Region > 3)

#simulated spins
tenspins <- spinner_data(areas,10)

manyspins <- spinner_data(areas, 1000)
bar_plot(manyspins)

#frequency table
S <- data.frame(table(manyspins))
colnames(S) <- c("Region", "N")
                
#(S <- summarise(group_by(data.frame(Region = manyspins), Region), N = n()))

########

(bayes_df <- data.frame(Model = paste("Spinner", c("A","B","C","D"))))

bayes_df$Prior <- rep(0.25, 4)

bayes_df$Likelihood <- round(c(1/3, 1/2, 1/4, 1/6), digits = 2)
#computes posterior variable col
bayes_df <- bayesian_crank(bayes_df)

#plot
prior_post_plot(bayes_df) 

#########

Test = c("Positive", "Negative")

(bayes_df <- data.frame(Model = Test))

prior_prob <- 0.001
likely_prob <- 0.99

#1st test
bayes_df$Prior <- c(prior_prob, 1-prior_prob)
bayes_df$Likelihood <- c(likely_prob, 1-likely_prob)

#computes posterior variable col
(bayes_df <- bayesian_crank(bayes_df))

#plot
prior_post_plot(bayes_df) 

####
#2nd test, follow up tests - update our prior belief!!!!
posteriortest <- bayes_df$Posterior 
bayes_df$Prior <- posteriortest

#computes posterior variable col
(bayes_df <- bayesian_crank(bayes_df))

#plot
prior_post_plot(bayes_df)
