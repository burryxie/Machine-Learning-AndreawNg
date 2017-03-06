setwd("K:/R/DL/ex2Data")
x <- read.table("ex2x.txt")
y <- read.table("ex2y.txt")
x$v2 <- 1
x <- as.matrix(x)
plot(x[,1],y$V1)
alpha <- 0.07
thelta <- data.frame(0,0)
names(thelta) <- c('thelta1','thelta0')
n <- dim(x)[1]


#function for thelta iteration
j_thelta <- function(max_inter = 1500){
  i = 1
  for(i in seq(1,max_inter)){
    j <- single_example(df=i)
    thelta_1 <- thelta[i,1] - alpha/n*(j %*% x[,1])
    thelta_2 <- thelta[i,2] - alpha/n*(j %*% x[,2])
    thelta <<- rbind(thelta,c(thelta_1,thelta_2))
    print(i)
    
  #  if(sum(abs(thelta[dim(thelta)[1],]-thelta[dim(thelta)[1]-1,])^2)<0.000001)
   #   {print('Converged')
    #  break}
  }
  print('Algorithms finished')
}


#compute the sum for the batch gradient descent
single_example <- function(df){
  j <- NULL
  i <- 1
  for(i in seq(1,dim(x)[1])){
    j <- c(j,as.matrix(thelta[df,])%*%x[i,]-y[i,])
  }
  return(j)
}

j_thelta()

#plot for comparision
evaluate <- data.frame(x=x[,1],y=y,type='training data')
evaluate <- rbind(evaluate,data.frame(x=x[,1],V1=x%*% as.matrix(thelta)[dim(thelta)[1],],type='predicted' )) 

library(ggplot2)
ggplot(evaluate,aes(x=x,y=V1,color=type))+geom_point()


#Better understanding for loss function
setwd("K:/R/DL/ex2Data")
x <- read.table("ex2x.txt")
y <- read.table("ex2y.txt")
x$v2 <- 1
thelta0 <- seq(-1,1,0.001)
thelta1 <- seq(-3,3,0.001)
result <- data.frame()

j_thelta <- function(t_0,t_1){
  i <- 1
  my_sum <- 0
  for(i in seq(1,dim(x)[1])){
    my_sum <- my_sum+(c(t_0,t_1) %*% as.matrix(x)[i,]-y[i,1])^2
  }
  return(c(t_0,t_1,my_sum))
}

for(i in thelta0){
  for(j in thelta1){
    result <- rbind(result,j_thelta(i,j))
  }
}