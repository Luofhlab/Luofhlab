options(stringsAsFactors = T)
args<-commandArgs(T)
if(length(args)<2){ stop("Rscrip roc.R k-fold dat1 dat2 ... \ndat:\n \tgroup\tspecies1\tspecise2\nsample1\tA\t12\t7\nsample2\tB\t12\t12", call.=FALSE)}

datlist <- list()
for (i in 2:length(args)) {
  
  datlist[[i-1]] <- read.table(args[i],header = T,row.names = 1)
  assign(paste("dat",(i-1),sep = ""),datlist[[i-1]])  
}
k = as.numeric(args[1])
n = length(args)-1
library(plyr)
library(randomForest)
library(ROCR)
library(pROC)
library(ggplot2)
library(ggsci)
##设置k折交叉验证
CVgroup <- function(k,datasize,seed){
  cvlist <- list()
  set.seed(seed)
  n <- rep(1:k,ceiling(datasize/k))[1:datasize]   
  temp <- sample(n,datasize)  
  x <- 1:k
  dataseq <- 1:datasize
  cvlist <- lapply(x,function(x) dataseq[temp==x]) 
  return(cvlist)
}

roclist<-list()
for (j in 1:n) {
  data <- datlist[[j]]
  datasize <- nrow(data)
  cvlist <- CVgroup(k = k,datasize = datasize,seed = 1000)
  pred <- data.frame()
  for (i in 1:k){
    train <- data[-cvlist[[i]],]  
    test <- data[cvlist[[i]],]
    model <-randomForest(group~.,data = train,ntree = 500)   
    prediction <- predict(model,subset(test,select = -group),type="prob")
    randomtree <- rep(500,length(prediction)/2)  
    kcross <- rep(i,length(prediction)/2)
    temp <- data.frame(cbind(subset(test,select = group),prediction,randomtree,kcross))
    pred <- rbind(pred,temp)   
  }
  roc_tem <- roc(pred$group,pred$A)
  assign(paste("r",j,sep = ""),roc_tem)   
  roclist[[paste("ROC",j,"(AUC = ",round(roc_tem$auc,4),")",sep = "")]] <- roc_tem
}
p <- ggroc(roclist,aes=c("linetype","color"),size=1) + theme_minimal() +
  geom_segment(aes(x = 1, xend = 0, y = 0, yend = 1), color="grey", linetype="dashed")


pdf("roc.pdf",width=10,height=6)
 
p

dev.off()




