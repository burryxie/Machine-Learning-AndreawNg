get_data <- function(filename){
  file <- read.table(filename)
 # print(t(file))
  return(t(file))
}


#loading nspam data for vocabulary construction
#part 1. trainning nspam
setwd("H:/R/DL/ex6DataEmails/nonspam-train")
nspam_tr <- list.files(all.files = TRUE)[-c(1:2)]

nspam_vocab_tr <- lapply(nspam_tr,get_data)
nspam_vocab_tr <- do.call('rbind',nspam_vocab_tr)

#part 2. test nspam
setwd("H:/R/DL/ex6DataEmails/nonspam-test")
nspam_te <- list.files(all.files = TRUE)[-c(1:2)]

nspam_vocab_te <- lapply(nspam_te,get_data)
nspam_vocab_te <- do.call('rbind',nspam_vocab_te)

#loading spam data for vocabulary construction
#part 1. trainning spam
setwd("H:/R/DL/ex6DataEmails/spam-train")
spam_tr <- list.files(all.files = TRUE)[-c(1:2)]

spam_vocab_tr <- lapply(spam_tr,get_data)
spam_vocab_tr <- do.call('rbind',spam_vocab_tr)

#part 2. test spam
setwd("H:/R/DL/ex6DataEmails/spam-test")
spam_te <- list.files(all.files = TRUE)[-c(1:2)]

spam_vocab_te <- lapply(spam_te,get_data)
spam_vocab_te <- do.call('rbind',spam_vocab_te)


#constructing all vocabulary
vocab <- rbind(nspam_vocab_tr,nspam_vocab_te,spam_vocab_tr,spam_vocab_te)
real_vocab <- data.frame(table(vocab[,1]))
real_vocab <- real_vocab[with(real_vocab, order(real_vocab$Freq,decreasing = TRUE)) ,] #set descending
real_vocab <- real_vocab[-which(real_vocab$Var1 %in% letters),] #remove single characters
real_vocab <- real_vocab[c(1:2500),]
names(real_vocab) <- c('word','freq')
rownames(real_vocab) <- 1:2500

rm(get_data,vocab,nspam_te,nspam_vocab_te,nspam_tr,nspam_vocab_tr,spam_te,spam_vocab_te,spam_tr,spam_vocab_tr)
