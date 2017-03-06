#mapping doc to featrue vector
feature <- function(filename){
  file <- t(read.table(filename,sep=' '))
  file <- file[!is.na(file)]
  file <- data.frame(table(file))
  names(file) <- c('word','count')
  file$'index_in_vocab' <- match(file$word,real_vocab$word)
  file <- file[with(file,order(count,decreasing =TRUE)),]
  file <- file[!is.na(file$index_in_vocab),]
  names(file) <- c('Word','Count','WordID')
  file$'DocID' <- match(filename,spam_te)
  file <- file[,c('DocID','WordID','Count')]
 
   return(file)
}


#loading nspam for feature vector construction
#part 1. trainning nspam
setwd("H:/R/DL/ex6DataEmails/nonspam-train")
nspam_tr <- list.files(all.files = TRUE)[-c(1:2)]
nspam_feature_tr <- lapply(nspam_tr,feature)
nspam_feature_tr <- do.call('rbind',nspam_feature_tr)
rm(nspam_tr)
write.table(nspam_feature_tr,'nspam_feature_tr.txt',row.names = FALSE)

#part 2. test nspam
setwd("H:/R/DL/ex6DataEmails/nonspam-test")
nspam_te <- list.files(all.files = TRUE)[-c(1:2)]
nspam_feature_te <- lapply(nspam_te,feature)
nspam_feature_te <- do.call('rbind',nspam_feature_te)
rm(nspam_te)
write.table(nspam_feature_te,'nspam_feature_te.txt',row.names = FALSE)


#loading nspam for feature vector construction
#part 1. trainning nspam
setwd("H:/R/DL/ex6DataEmails/spam-train")
spam_tr <- list.files(all.files = TRUE)[-c(1:2)]
spam_feature_tr <- lapply(spam_tr,feature)
spam_feature_tr <- do.call('rbind',spam_feature_tr)
rm(spam_tr)
write.table(spam_feature_tr,'spam_feature_tr.txt',row.names = FALSE)


#part 2. test nspam
setwd("H:/R/DL/ex6DataEmails/spam-test")
spam_te <- list.files(all.files = TRUE)[-c(1:2)]
spam_feature_te <- lapply(spam_te,feature)
spam_feature_te <- do.call('rbind',spam_feature_te)
rm(spam_te)
write.table(spam_feature_te,'spam_feature_te.txt',row.names = FALSE)


rm(feature)



#construct large matrix
large_matrix <- function(file){
  the_matrix <- matrix(nrow=130,ncol=2500)
  for(i in 1:nrow(file)){
    the_matrix[file[i,1],file[i,2]] <- file[i,3]
  }
  return(the_matrix)
}


setwd("K:/R/DL/ex6DataEmails/nonspam-train")
file <- read.table('nspam_feature_tr.txt',header = TRUE)
nspam_tr <- large_matrix(file)
rm(file)
write.table(nspam_tr,'nspam_tr.txt',row.names = FALSE)

setwd("K:/R/DL/ex6DataEmails/nonspam-test")
file <- read.table('nspam_feature_te.txt',header = TRUE)
nspam_te <- large_matrix(file)
rm(file)
write.table(nspam_te,'nspam_te.txt',row.names = FALSE)

setwd("K:/R/DL/ex6DataEmails/spam-train")
file <- read.table('spam_feature_tr.txt',header = TRUE)
spam_tr <- large_matrix(file)
rm(file)
write.table(spam_tr,'spam_tr.txt',row.names = FALSE)

setwd("K:/R/DL/ex6DataEmails/spam-test")
file <- read.table('spam_feature_te.txt',header = TRUE)
spam_te <- large_matrix(file)
rm(file)
write.table(spam_te,'spam_te.txt',row.names = FALSE)


