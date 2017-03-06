#load data
x <- read.table('ex3x.dat')
x$v3 <- 1
x <- x[,c('v3','V1','V2')]
y <- read.table('ex3y.dat')

#pre-processing, feature scaling
x$V1 <- (x$V1-mean(x$V1))/sd(x$V1)
x$V2 <- (x$V2-mean(x$V2))/sd(x$V2)
n <- dim(x)[1]

#initialization
#thelta_record <- data.frame()
thelta <- c(0,0,0)
#j_thelta <- t(as.matrix(x) %*% thelta -y) %*% as.matrix(as.matrix(x) %*% thelta -y)
#thelta_record <- rbind(thelta_record,thelta)

alpha_to_be_decided <- seq(0.01,10,0.1)


#choose the right learning rate
j_thelta1 <- function(alpha1){
  max_iteration <- 100
  j_thelta <- data.frame()
  for(i in seq(1,max_iteration)){
    loss <- t(as.matrix(x) %*% thelta -y) %*% as.matrix(as.matrix(x) %*% thelta -y)
    j_thelta <- rbind(j_thelta,c(alpha1,i,loss))
    sum_over_all <- as.matrix(x) %*% thelta - y
    thelta[1] <<- thelta[1]-alpha1/n*(as.matrix(sum_over_all)[,1] %*% as.matrix(x)[,1])
    thelta[2] <<- thelta[2]-alpha1/n*(as.matrix(sum_over_all)[,1] %*% as.matrix(x)[,2])
    thelta[3] <<- thelta[3]-alpha1/n*(as.matrix(sum_over_all)[,1] %*% as.matrix(x)[,3])
  }
  #print(alpha1)
  #names(j_thelta) <- c('alphas','iterations','loss')
  #return(j_thelta)  
  print(thelta)
}

result <- lapply(c(0.01,0.03,0.1,0.3,1,1.3),j_thelta1)
result1 <- do.call('rbind',result)
result1$alphas <- as.factor(result1$alphas)


#plot the loss vs number of iterations
result2 <- result1[which(result1$alphas %in% c(0.01,0.03,0.1,0.3,1,1.3)),]
library(ggplot2)
ggplot(result1,aes(x=iterations,y=loss,color=alphas))+geom_line()+
  xlim(c(0,50))+theme(scale_colour_manual(values = c('0.01'='black','0.03'='yellow','0.10'='red','0.30'='green','1.00'='blue','1.30'='darkgreen')))


j_thelta1(1)
