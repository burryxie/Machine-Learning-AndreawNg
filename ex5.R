#linear part
x <- read.table('ex5Linx.dat')
y <- read.table('ex5Liny.dat')
x$V0 <- 1
x$V2 <- x$V1^2
x$V3 <- x$V1^3
x$V4 <- x$V1^4
x$V5 <- x$V1^5
x <- as.matrix(x[,c('V0','V1','V2','V3','V4','V5')])
y <- as.matrix(y)

lambada <- 10
thelta <- solve(t(x) %*% x + lambada * diag(c(0,rep(1,5)))) %*% t(x) %*%y
norm(thelta)

rm(list=ls())


#===========================================================================
#logistic part
x <- read.table('ex5Logx.dat',sep=',')
y <- read.table('ex5Logy.dat')
y <- as.matrix(y)
library(ggplot2)
my_data <- cbind(x,y)
names(my_data) <- c('x1','x2','y')
ggplot(my_data,aes(x=x1,y=x2,color=as.factor(y)))+geom_point()


#feature mapping
feature_mapping <- function(x1,max_power){
  d <- dim(x1)[2]
  new_x <- data.frame(rep(1,dim(x1)[1]))
  for(i in seq(1,max_power)){
    all <- 1
    for(j in seq(0,i)){
      new_x <- cbind(new_x, x1$V1^j*x1$V2^(i-j))
      all <- all+1
      print(c(i,j,i-j))
        
    }
  }
  return(new_x)
}

x <- feature_mapping(x,6)
names(x) <- paste('x',seq(0,27),sep='')
x <- as.matrix(x)
rm(feature_mapping)


#initialization
thelta <- rep(0,28)

sigmoid <- function(x){
  return(1/(1+exp(-x)))
}

loss <- function(thelta){
  loss_1 <- -1/dim(x)[1]*(t(y) %*% log10(sigmoid(x %*% thelta)) + t(1-y) %*% log10(1-sigmoid(x %*% thelta))) + lambada/(2*dim(x)[1])*sum(thelta^2)
  return(loss_1)
}

loss_record <- data.frame()

iteration <- function(lambada,max_iteration){
  for(s in 1: max_iteration){
    loss_record <- rbind(loss_record,c(s,loss(thelta)))
    
    #j matrix
    j<-c()
    for(i in 1:28){
      j <- c(j,sum(x[,i] * (sigmoid(x %*% thelta)-y))+lambada/dim(x)[1]*thelta[i])
    }
    
    #hessian matrix
    h <- matrix(rep(0,28*28),28,28)
    for(i in 1:dim(x)[1]){
      h <- h+ as.vector(sigmoid(x[i,] %*% thelta) * (1- sigmoid(x[i,] %*% thelta))) * as.matrix(x[i,]) %*% t(as.matrix(x[i,]))
    }
    h <-  1/dim(x)[1] * h+ lambada/dim(x)[1] * diag(c(0,rep(1,27)))
    
    #iteration
    thelta <- thelta - solve(h) %*% as.matrix(j)
    
  }
  return(thelta)
}