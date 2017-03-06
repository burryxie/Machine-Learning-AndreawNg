x <- read.table('ex4x.dat')
x$V3 <- 1
x <- t(x)
y <- read.table('ex4y.dat')
n <- dim(x)[2]


#sifmoid function
sigmoid <- function(x){
  return(1/(1+exp(-x)))
}

#loss function
loss <- function(thelta){
  current_loss <- 1/n*(sum((-y) * log10(sigmoid(thelta %*% x)))-
                      sum((1-y) * log10(1-sigmoid(thelta %*% x))))
  return(current_loss)
}

loss_record <- data.frame()
thelta <- t(c(0,0,0))

#for hessian
iteration <- function(){
  for(i in seq(1,20)){
    current_loss1 <- loss(thelta)
    loss_record <<-rbind(loss_record,c(i,current_loss1))
    
    m <- 0
    for(i in seq(1,n)){
      m <- m+(sigmoid(sum(thelta %*% x[,i])) * (1-sigmoid(sum(thelta %*% x[,i]))) * x[,i] %*% t(x[,i]))
    }
    
    j <- 0
    for(i in seq(1,n)){
      j <-j+(sigmoid(sum(thelta %*% x[,i])) - y[i,])*x[,i] 
    }
    j <- 1/n*j 
    thelta <<- t(t(thelta)-solve(1/n*m) %*% j)
  }
  return(thelta)
}

iteration()
names(loss_record) <- c('iterations','loss')


library(ggplot2)
ggplot(loss_record,aes(x=iterations,y=loss))+geom_line()+xlim(c(0,6))
