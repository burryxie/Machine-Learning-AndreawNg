#naive bayes modeling
setwd("K:/R/DL/ex6DataEmails/spam-train")
spam_tr <- read.table('spam_tr.txt',header = TRUE)
setwd("K:/R/DL/ex6DataEmails/spam-test")
spam_te <- read.table('spam_te.txt',header = TRUE)
setwd("K:/R/DL/ex6DataEmails/nonspam-train")
nspam_tr <- read.table('nspam_tr.txt',header = TRUE)
setwd("K:/R/DL/ex6DataEmails/nonspam-test")
nspam_te <- read.table('nspam_te.txt',header = TRUE)

p_spam  <- dim(spam_tr)[1]/(dim(spam_tr)[1]+dim(nspam_tr)[1])
p_nspam <- 1-p_spam

spam_ni  <- rowSums(spam_tr,na.rm = TRUE)
nspam_ni <- rowSums(nspam_tr,na.rm = TRUE)
spam_k   <- colSums(spam_tr,na.rm = TRUE)+1
nspam_k  <- colSums(nspam_tr,na.rm = TRUE)+1

p_k_spam  <- spam_k/(sum(spam_ni)+2500)
p_k_nspam <- nspam_k/(sum(nspam_ni)+2500)

spam_te  <- as.matrix(spam_te)
nspam_te <- as.matrix(nspam_te)

test_model <- function(k){
  te <- spam_te[k,]
  te_spam <- '^'(p_k_spam,te)
  te_spam <- sum(log10(te_spam),na.rm=TRUE)+log10(0.5)
  te_nspam <-'^'(p_k_nspam,te)
  te_nspam <- sum(log10(te_nspam),na.rm=TRUE)+log10(0.5)
  result <- ifelse(te_spam > te_nspam,1,0)
  return(result)
}

pred_result_spam <- lapply(1:130,test_model)
pred_result_spam <- do.call('c',pred_result_spam)
err_rate_spam <- sum(pred_result_spam != rep(1,130))/130*100

pred_result_nspam <- lapply(1:130,test_model)
pred_result_nspam <- do.call('c',pred_result_nspam)
err_rate_nspam <- sum(pred_result_nspam != rep(0,130))/130*100

err_rate <- sum(c(rep(c(1,0),each=130))!=c(pred_result_spam,pred_result_nspam))/260*100
