#https://www.datacamp.com/community/tutorials/pipe-r-tutorial

#Piping in R

x <- rnorm(100, 0)

plot(x)

library(magrittr)
plot(x %>% sort %>% rev)

 x %<>% abs

###
# NOTE: %T>% returns the value on the left after plotting a graph, nice to have.
rnorm(200) %>% matrix(ncol = 2) %T>% plot %>% colSums()


6 %>% round(132.323432234234234234, digits = .)

paste(1:5, letters[1:5])

1:5 %>% paste(., letters[.])

