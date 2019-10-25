# Parallel Computing using foreach package
rm(list = ls())

library(foreach) #credits to revolutionAnalytics
library(iterators)
library(ISLR)
library(ggplot2)
library(doParallel) #for registering multicore machine/cluster 
#=======================================================================
# the foreach language - some basics

foreach(i=1:10) %do% rnorm(1)
foreach(icount(10)) %do% rnorm(1) #same operation as above

foreach(icount(10), .combine = "+") %do% rnorm(1) #adds items as they...
                                                  # ... come in
foreach(icount(10), .combine = "sum") %do% rnorm(1) # same as above

foreach(i=1:10, .combine = "c") %do% rnorm(1)
foreach(i=1:7, .combine = "rbind") %do% quantile(rnorm(400))

# many other functions can be used to combine the results

#=======================================================================
# Example: bootstrapping
#=======================================================================
data("Auto")

trials <- 1000 # the number of bootstraps
# try first with 1000 then increase N

#-----------------------------------------------------------------------
# using one core ... normal way to do bootstrap
#-----------------------------------------------------------------------
stime1_core <- system.time({
  r <- foreach(icount(trials), .combine=rbind) %do% {
    ind <- sample(392, 392, replace=TRUE) #selects the bootstrap sample
    coefficients(lm(mpg~ cylinders + displacement + horsepower + weight +
                      acceleration, data = Auto[ind,]))
  }
})[3]
stime1_core



#-----------------------------------------------------------------------
#Using 2 Cores
#-----------------------------------------------------------------------
cl <- makeCluster(2)
registerDoParallel(cl)
getDoParWorkers()
stime2_cores <- system.time({
  r <- foreach(icount(trials), .combine=rbind) %dopar% {
    ind <- sample(392, 392, replace=TRUE) #selects the bootstrap sample
    coefficients(lm(mpg~ cylinders + displacement + horsepower + weight +
                      acceleration, data = Auto[ind,]))  }
})[3]
stime2_cores
stopCluster(cl)
#-----------------------------------------------------------------------
# On 4 cores
#-----------------------------------------------------------------------
cl <- makeCluster(4)
registerDoParallel(cl)
getDoParWorkers()
stime4_cores <- system.time({
  r <- foreach(icount(trials), .combine=rbind) %dopar% {
    ind <- sample(392, 392, replace=TRUE) #selects the bootstrap sample
    coefficients(lm(mpg~ cylinders + displacement + horsepower + weight +
                      acceleration, data = Auto[ind,])) }
})[3]
stime4_cores
stopCluster(cl)

r <- as.data.frame(r)
ggplot(r,aes(x = cylinders)) + geom_density()
ggplot(r,aes(x = displacement)) + geom_density()
ggplot(r,aes(x = horsepower)) + geom_density()
ggplot(r,aes(x = weight)) + geom_density()
ggplot(r,aes(x = acceleration)) + geom_density()



foreach(i=iter(Auto[,-(8:9)],by = "col"),.combine = "rbind") %do% {
  mu = mean(i)
  .sd = sd(i)
  c(mu,.sd)
}



big_data <- data.frame(x = rnorm(1e5))
big_data$y <- big_data$x + runif(1e5,0,0.1)

foreach(i=1:2, .combine = "c") %do% min(g_vec[i])

foreach(a=3:5, .combine = "cbind") %:% 
  foreach(b=6:8, .combine = "c") %do% {
    b^a
  }











