source('simulation.r')
getwd()
# system information
proc_time <- 0.001
queue_speed <- 275000
trans_speed <- 150000
chan_length <- 100*(1:4)
horizon <- 30
path <- list()
path[[1]] <- c(1, 5, 2)
path[[2]] <- c(1, 5, 2, 6, 3)
path[[3]] <- c(1, 5, 2, 7, 4)
path[[4]] <- c(2, 5, 1)
path[[5]] <- c(2, 6, 3)
path[[6]] <- c(2, 7, 4)
path[[7]] <- c(3, 6, 2, 5, 1)
path[[8]] <- c(3, 6, 2)
path[[9]] <- c(3, 8, 4)
path[[10]] <- c(4, 7, 2, 5, 1)
path[[11]] <- c(4, 7, 2)
path[[12]] <- c(4, 8, 3)

xsize <- 100
yrep <- 5

# true input model
# input_true <- c(60, 40, 50, 80, 65, 20, 100, 22, 26, 40, 50, 60, 1/300)
# input_true <- c(50, 30, 40, 60, 55, 20, 70, 22, 26, 35, 40, 50, 1/300)
# input_true <- c(40, 30, 35, 50, 45, 15, 60, 15, 20, 25, 30, 40, 1/300)
# input_true <- c(50, 40, 45, 60, 55, 25, 70, 25, 30, 35, 40, 50, 1/300)

basx <- runif(n = xsize, min = 200, max = 600)
basl1 <- runif(n = xsize, min = 10, max = 100)
basl2 <- runif(n = xsize, min = 10, max = 100)
basl3 <- runif(n = xsize, min = 10, max = 100)
print(basx)
#arr = array(1:1, dim = c(xsize*yrep, 5))
arr = array(1:1, dim = c(xsize*yrep, 5, 50))
#arr[120,1] <- 3
print(arr)

# set seed for rng
#set.seed(322277)
# compute the true value
for (j in 1:50){
  for (i in 1:yrep) {
    print(i)
    for (t in seq(1,xsize,1)) {
      x=basx[t]
      print(x)
      input_true <- c(basl1[t], basl2[t], basl3[t], 50, 45, 15, 60, 15, 20, 25, 30, 40, 1/x)
      truth <- get_truth(input_true, 1)
      #save(truth, file = 'truth.RData')
      truth <- truth$mean
      arr[(i-1)*xsize+t,1,j] <- x
      arr[(i-1)*xsize+t,2,j] <- basl1[t]
      arr[(i-1)*xsize+t,3,j] <- basl2[t]
      arr[(i-1)*xsize+t,4,j] <- basl3[t]
      arr[(i-1)*xsize+t,5,j] <- truth
    }
  }
}
print('finish computing truth')
print(arr)
write.csv(arr, file = "truth_4input_tr2.csv")

