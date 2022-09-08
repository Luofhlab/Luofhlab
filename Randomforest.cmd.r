
library(randomForest,warn.conflicts = F)
library(maptools,warn.conflicts = F)
library(pROC)
basename="randomForest"
mycol <-c("#CD0000","#3A89CC","#769C30","#D99536","#7B0078","#BFBC3B","#6E8B3D","#00688B","#C10077","#CAAA76","#EEEE00","#458B00","#8B4513","#008B8B","#6E8B3D","#8B7D6B","#7FFF00","#CDBA96","#ADFF2F")
#mypch <-c(21,22,24,23,25,11,13,8)
mypch <-c(21:25,3,4,7,9,8,10,15:18,0:14)
pch=16
lcol="#1E90FF"
col="#1E90FF"

# if read otu data
otu_ori <-read.table("ALL_new_species.xls",sep="\t",head=T,check.names = F)
rownames(otu_ori) <-as.factor(otu_ori[,1])
otu_ori <-otu_ori[,-1]
rownames(otu_ori) <-sapply(rownames(otu_ori),function(x) gsub("_*{.+}"," ",x,perl = TRUE))
rownames(otu_ori) <-sapply(rownames(otu_ori),function(x) gsub("-","_",x,perl = TRUE))
rownames(otu_ori) <-sapply(rownames(otu_ori),function(x) gsub("\\[","",x,perl = TRUE))
rownames(otu_ori) <-sapply(rownames(otu_ori),function(x) gsub("\\]","",x,perl = TRUE))
rownames(otu_ori) <-sapply(rownames(otu_ori),function(x) gsub("\\(","",x,perl = TRUE))
rownames(otu_ori) <-sapply(rownames(otu_ori),function(x) gsub("\\)","",x,perl = TRUE))
rownames(otu_ori) <-sapply(rownames(otu_ori),function(x) gsub("^[0-9]","X\\1",x,perl = TRUE))
rownames(otu_ori) <-sapply(rownames(otu_ori),function(x) gsub("/","",x,perl = TRUE))
otu_ori <-as.data.frame(t(otu_ori),stringsAsFactors=T)

map="map"
if(map !="none"){
                sd <-read.table("map",head=T,sep="\t",comment.char = "",check.names = FALSE)        
                rownames(sd) <- as.character(sd[,1])
                sd[,1] <-as.character(sd[,1])
                otu <- otu_ori[rownames(sd),]
                #sd$group <-as.factor(sd$group )
                legend <- as.matrix(unique(sd$group)) 
}else{ otu <-otu_ori } 
grp <- sd[as.character(rownames(otu)),2]

set.seed(123)
if(map != "none"){
	otu.rf <- randomForest(grp ~ .,otu,importance=T,proximity=T,ntree=500)

	class_count <-as.matrix(table(grp))
	class_color  <-mycol[1:(length(class_count))]
	class_pch <- mypch[1:(length(class_count))]
	class <-data.frame(count=class_count,color=as.character(class_color),pch=class_pch)
	col=as.character(class[grp,]$color)
	pch=class[grp,]$pch
        
        ############ all variable RandomFoest
	#mds = paste("randomForest/",basename,".ALL_new_species_group_mds.pdf",sep="")
	#pdf(mds,width=8,height=8)
	#otu.mds <- MDSplot(otu.rf,grp,palette="white",pch=pch,k=2,bg=paste(col,"FF",sep=""))
        #title("MDS Plot of Random Forest",font=2,cex=1)
	#if(length(legend)>1){
	#legend("topright",legend=rownames(class_count),pch=class_pch,col=class_color,pt.bg=paste(class_color,"FF",sep=""))
	#}
	#if(T==T){
	#pointLabel(x = otu.mds$points[, 1], y = otu.mds$points[, 2], labels = rownames(otu.mds$points), cex =  0.1 + 1/log10(nrow(sub.otu.mds$points)), col = col)
	#}

	##randomforest classification table
	rf_table <- paste("randomForest/",basename,".ALL_new_species_confusion_table.xls",sep="")
	write.table(otu.rf$confusion,rf_table,sep="\t",quote=F)
}else{
	otu.rf <- randomForest(otu,importance=T,proximity=T,ntree=500)
	
        #mds = paste("randomForest/",basename,".ALL_new_species_mds.pdf",sep="")
	#pdf(mds,width=8,height=8)
	#otu.mds <- MDSplot(otu.rf,"1",palette=class_color,pch=pch,k=2)
        #title("MDS Plot of Random Forest",font=2,cex=1)
	#if(T==T){
	#pointLabel(x = otu.mds$points[, 1], y = otu.mds$points[, 2], labels = rownames(otu.mds$points), cex =  0.1 + 1/log10(nrow(sub.otu.mds$points)), col = col)
	#}
}

#dev.off()

#################steps ######### 50:step1;  50-100:step2=proportion1;  100+:step3=proportion2
mtry1 <- function(p) max(1, floor(sqrt(p)))
n <- length(grp)
p <- ncol(otu)

steps <- c(-1,0.8,0.7)
step1 <- steps[1]
step2 <- steps[2]
step3 <- steps[3]
if (p>=50){
    n.var1 <- seq(50,1,by=step1)
    if (p>=100){
      k1 <- floor(log(100, base = 1/step2))
      n.tmp <- round(100 * step2^(0:(k1 - 1)))
      same <- diff(n.tmp) == 0
      if (any(same)) {n.tmp <- n.tmp[-which(same)]}
      n.tmp <- n.tmp[which(n.tmp>50)]
      n.var2<-c(n.tmp,n.var1)
      k2 <- floor(log(p, base = 1/step3))
      n.tmp <- round(p * step3^(0:(k2 - 1)))
      same <- diff(n.tmp) == 0
      if (any(same)) {n.tmp <- n.tmp[-which(same)]}
      n.tmp <- n.tmp[which(n.tmp>100)]
      n.var<-c(n.tmp,n.var2)
      }else{
        k1 <- floor(log(p, base = 1/step2))
        n.tmp <- round(p * step2^(0:(k1 - 1)))
        n.tmp <- n.tmp[which(n.tmp>50)]
        n.var<-c(n.tmp,n.var1)
      }
  }else{
    n.var <- seq(p,1,by=-1) 
  }  


################# RF 10-fold cross validtion
my_rfcv <- function (trainx, trainy, cv.fold, n.var , imp_type , mtry = mtry1(p), scale, ntree) 
{ 
  classRF <- is.factor(trainy)
  k <- length(n.var)
  cv.pred <- vector(k, mode = "list")
  for (i in 1:k) cv.pred[[i]] <- trainy
  if (classRF) {
    f <- trainy
  }else {
    f <- factor(rep(1:5, length = length(trainy))[order(order(trainy))])
  }
  nlvl <- table(f)
  idx <- numeric(n)
  for (i in 1:length(nlvl)) {
    idx[which(f == levels(f)[i])] <- sample(rep(1:cv.fold,length = nlvl[i]))
  }

  for (i in 1:cv.fold) {
    all.rf <- randomForest(trainx[idx != i, , drop = FALSE], 
                           trainy[idx != i], trainx[idx == i, , drop = FALSE], 
                           trainy[idx == i], mtry = mtry, importance = TRUE,ntree=ntree)
    cv.pred[[1]][idx == i] <- all.rf$test$predicted
    impvar <- (1:p)[order(importance(all.rf,scale=scale)[, imp_type], decreasing = TRUE)]
    for (j in 2:k) {
      imp.idx <- impvar[1:n.var[j]]
      sub.rf <- randomForest(trainx[idx != i, imp.idx, drop = FALSE], trainy[idx != i], 
                             trainx[idx == i, imp.idx, drop = FALSE], trainy[idx == i], 
                             mtry = mtry1(n.var[j]), ntree = ntree)
      cv.pred[[j]][idx == i] <- sub.rf$test$predicted
      NULL
    }
    NULL
  }
  if (classRF) {
    error.cv <- sapply(cv.pred, function(x) mean(trainy != x))
  }else {
    error.cv <- sapply(cv.pred, function(x) mean((trainy - x)^2))
  }
  names(error.cv) <- names(cv.pred) <- n.var
  list(n.var = n.var, error.cv = error.cv, prlibraryedicted = cv.pred)
}

method <- "AUC"

if (method=="CV"){
	error <- c()
	num <- c()
	multimes=20
        type <- "2"
        if (type==1){
        	imp_type <-"MeanDecreaseAccuracy"
	}else if(type==2){
        	imp_type <- "MeanDecreaseGini"
	}else{imp_type <- type}
	for(i in 1:multimes){
	rfcv <- my_rfcv(otu,grp,10,n.var,imp_type,mtry1(p),T,500)
	each <- rfcv$error.cv
	error <- cbind(error,each)
	}
	meanerror <- as.data.frame(rowMeans(error))
	meanerror[,2] <- rownames(meanerror)
	colnames(meanerror) <-c("MeanError","var")
	meanerror[,2] <- as.integer(meanerror[,2])
	#min_positon <- meanerror[tail(which(meanerror==min(meanerror)),1),]
	#choose_var_num <- min_positon[2]

	cross_file <- paste("randomForest/",basename,".ALL_new_species_10-fold_CV.xls",sep="")
	new_meanerror <- cbind(meanerror[,2],meanerror[,1])
	colnames(new_meanerror) <-c("var","MeanError")
	new_meanerror[,2] <- round(new_meanerror[,2],3)
	write.table(new_meanerror,file=cross_file,sep="\t",row.names=F)
         
        ########## change coordinates
        max_co <- round(max(meanerror$var)/100+0.5)*100
        top50 <- c(1,10,20,30,40,50)
        others <- seq(100,max_co,by=100)
        plot_x_labels <- c(top50,others)
        change_x <- meanerror$var[meanerror$var>50]/20 + 50
        all_x <- c(change_x,meanerror$var[meanerror$var<=50])
        change_site <- others/20 + 50
        sites <- c(top50,change_site) 
        meanerror_change <- meanerror
        meanerror_change[,2] <- all_x
        min_positon <- meanerror_change[tail(which(meanerror_change==min(meanerror_change)),1),] 
        act_position <- meanerror[tail(which(meanerror_change==min(meanerror_change)),1),]
        choose_var_num <- act_position[2]

	choose_var <- paste("randomForest/",basename,".ALL_new_species_10-fold_CV.pdf",sep="")
	pdf(choose_var,8,8)
        
	plot(all_x,meanerror$MeanError,xaxt="n",type="o", lwd=2,col="red",cex=0.8,ann=FALSE)
        axis(side=1,at=sites,labels=plot_x_labels)
	points(min_positon[2],min_positon[1],pch=20,cex=1.5)
	min_point <- paste("(",act_position[2],",",round(min_positon[1],3),")",sep="")
	text(min_positon[2],min_positon[1],min_point,pos=4,cex=0.8,font=2)
	title("10-fold Cross Validation of Random Forest",xlab="variable numbers",ylab="error rate",font=2,cex=1)
	dev.off()
}


##################### AUC
my_AUC <- function(otu.rf,otu,label,prelabel,n.var,AUC_de,imp_type,scale,ntree){
   AUC <- c()
   k <- length(n.var)
   grp1 <- as.data.frame(grp)
   roc_data <- cbind(grp1,prelabel)
   AUC_tmp <- as.matrix(auc(roc(roc_data[,1],as.numeric(roc_data[,2]))))
   names(AUC_tmp) <- n.var[1]
   AUC <- c(AUC,AUC_tmp)
   impvar <- order(importance(otu.rf,scale=scale)[, imp_type], decreasing = TRUE)
   for (j in 2:k) {
   	imp.idx <- impvar[1:n.var[j]]
   	roc.sub <- randomForest(otu[,imp.idx, drop = FALSE], label, 
                             mtry = mtry1(n.var[j]), ntree = ntree)
        if(AUC_de=="class"){
        	sub.pre <- roc.sub$predicted
        	sub.pre1 <- as.matrix(sub.pre)
        	sub.pre1[sub.pre1==levels(pre)[1]] <- 0 
        	sub.pre1[sub.pre1==levels(pre)[2]] <- 1 
  	}else{
        	sub.pre <- roc.sub$votes[,1,drop=F]
      	}
   	sub.roc_data <- cbind(grp1,sub.pre)
   	sub.AUC_tmp <- as.matrix(auc(roc(sub.roc_data[,1],as.numeric(sub.roc_data[,2]))))
   	names(sub.AUC_tmp) <- n.var[j]
   	AUC <- c(AUC,sub.AUC_tmp)
    }
   return(AUC)
}

if (method=="AUC"){
  AUC_de <- "probability"
  if(AUC_de=="class"){
 	pre <- otu.rf$predicted
  	pre1 <- as.matrix(pre)
  	pre1[pre1==levels(pre)[1]] <- 0 
  	pre1[pre1==levels(pre)[2]] <- 1 
  }else{
  	pre <- otu.rf$votes[,1,drop=F]
      }
  type <- "2"
  if (type==1){
                imp_type="MeanDecreaseAccuracy"
        }else if(type==2){
                imp_type="MeanDecreaseGini"
        }else{imp_type <- type}

   AUC <- my_AUC(otu.rf,otu,grp,pre,n.var,AUC_de,imp_type,T,500)
   AUC <- as.data.frame(AUC)
   AUC[,2] <- as.numeric(rownames(AUC))
   colnames(AUC)[2]<- c("variable numbers")
   AUC_file <- paste("randomForest/",basename,".ALL_new_species_AUC.xls",sep="")
   write.table(AUC,AUC_file,sep="	",row.names=F)
   ######################  change coordinates 
   AUC[,2] <- as.numeric(AUC[,2])
   max_co <- round(max(AUC[,2])/100+0.5)*100
   top50 <- c(1,10,20,30,40,50)
   others <- seq(100,max_co,by=100)
   plot_x_labels <- c(top50,others)
   change_x <- AUC[AUC[,2]>50,2]/20 + 50
   all_x <- c(change_x,AUC[AUC[,2]<=50,2])
   change_site <- others/20 + 50
   sites <- c(top50,change_site) 
   AUC_change <- AUC
   AUC_change[,2] <- all_x
   max_position <- AUC_change[tail(which(AUC_change[,1]==max(AUC_change[,1])),1),]
   act_position <- AUC[tail(which(AUC_change[,1]==max(AUC_change[,1])),1),]

   choose_var_num <- act_position[2]
   choose_var <- paste("randomForest/",basename,".ALL_new_species_AUC.pdf",sep="")
   pdf(choose_var,8,8)
   plot(all_x,AUC_change[,1], xaxt="n",type="o", lwd=2,col="red",cex=0.8,ann=FALSE)
   axis(side=1,at=sites,labels=plot_x_labels)
   points(max_position[2],max_position[1],pch=20,cex=1.5)
   max_point <- paste("(",act_position[2],",",round(max_position[1],3),")",sep="")
   text(max_position[2],max_position[1],max_point,pos=4,cex=0.8,font=2)
   title("AUC of Sub_Random Forest",xlab="variable numbers",ylab="AUC",font=2,cex=1)
   dev.off()
}

#################
top = 0
if(top !=0){
choose_top <-  top
}else{choose_top <- as.integer(choose_var_num)  }

vimp <- paste("randomForest/",basename,".ALL_new_species_top",choose_top,"_vimp.pdf",sep="")
pdf(vimp,width=6,height=7)

type <- "2"
if(type == 1 | type== 2){
        varImpPlot(otu.rf,cex = 0.3 + 1/log2(choose_top),n.var = min(nrow(otu.rf$importance),choose_top),sort=T,main= " Dotchart of variable importance",scale=T,type=type,color="green4",pch=19) 
}else{
	type_gr <- "2"
        varImpPlot(otu.rf,cex = 0.3+ 1/log2(choose_top) ,n.var = min(nrow(otu.rf$importance),choose_top),sort=T,main= " Dotchart of variable importance",class=type_gr,type=1,color="green4",pch=19)
	}
dev.off()

########### choosed importance variables construct randomFoest
if (type==1){
	imp_type="MeanDecreaseAccuracy"
}else if(type==2){
	imp_type="MeanDecreaseGini"
}else{
	imp_type="2"
}

impvar <- order(importance(otu.rf,scale=T)[,imp_type], decreasing = TRUE)
choose_otu <- impvar[1:choose_top]
if(map != "none"){
sub.RF <- randomForest(grp ~ .,otu[,choose_otu, drop = FALSE], proximity=T,ntree = 500)
        mds = paste("randomForest/",basename,".ALL_new_species_subRF_pcoa.pdf",sep="")
        pdf(mds,width=8,height=8)
        sub.otu.mds <- MDSplot(sub.RF,grp,palette="white",pch=pch,k=2,bg=paste(col,"FF",sep=""))
        title("Pcoa Plot of Random Forest",font=2,cex=1)	

        if(length(legend)>1){
        legend("topright",legend=rownames(class_count),pch=class_pch,col=class_color,pt.bg=paste(class_color,"FF",sep=""))
        }

        if(T==T){
 	pointLabel(x = sub.otu.mds$points[, 1], y = sub.otu.mds$points[, 2], labels = rownames(sub.otu.mds$points), cex =  0.1 + 1/log10(nrow(sub.otu.mds$points)), col = col)
        }
        rf_table <- paste("randomForest/",basename,".ALL_new_species_confusion_subRF_table.xls",sep="")
        write.table(sub.RF$confusion,rf_table,sep="\t",quote=F)
}else{  
	sub.RF <- randomForest(otu[,choose_otu, drop = FALSE], proximity=T,ntree = 500)
        mds = paste("randomForest/",basename,".ALL_new_species_subRF_pcoa.pdf",sep="")
        pdf(mds,width=8,height=8)
        sub.otu.mds <- MDSplot(sub.RF,"1",palette=class_color,pch=pch,k=2)
        title("Pcoa Plot of Random Forest",font=2,cex=1)

        if(T==T){
        pointLabel(x = sub.otu.mds$points[, 1], y = sub.otu.mds$points[, 2], labels = rownames(sub.otu.mds$points), cex =  0.1 + 1/log10(nrow(sub.otu.mds$points)), col = col)
        }
}
dev.off()

################ predict new Data wtih constrcted RandomForest
pre = "none"
if(pre!="none"){
   newData <- read.table(pre,header=T,row.names=1,sep="\t",comment.char = "",check.names = FALSE)
   rownames(newData) <-sapply(rownames(newData),function(x) gsub("_*{.+}"," ",x,perl = TRUE))
   rownames(newData) <-sapply(rownames(newData),function(x) gsub("-","_",x,perl = TRUE))
   rownames(newData) <-sapply(rownames(newData),function(x) gsub("\\[","",x,perl = TRUE))
   rownames(newData) <-sapply(rownames(newData),function(x) gsub("\\]","",x,perl = TRUE))
   rownames(newData) <-sapply(rownames(newData),function(x) gsub("\\(","",x,perl = TRUE))
   rownames(newData) <-sapply(rownames(newData),function(x) gsub("\\)","",x,perl = TRUE))
   rownames(newData) <-sapply(rownames(newData),function(x) gsub("^[0-9]","X\\1",x,perl = TRUE))
   rownames(newData) <-sapply(rownames(newData),function(x) gsub("/","",x,perl = TRUE))
   newData2 <- as.data.frame(t(newData))
   if(1==1){
   	predict.vote <- as.data.frame(predict(otu.rf,newData2, type="vote"))
   	predict.class <- predict(otu.rf,newData2, type="response")
   	predict <- cbind(predict.class,predict.vote) 
   	predict_table <- paste("randomForest/",basename,".ALL_new_species_predict.xls",sep="")
   	write.table(predict,predict_table,sep = "\t" ,quote=F)
	}else{
        predict.vote <- as.data.frame(predict(sub.RF,newData2, type="vote"))
        predict.class <- predict(sub.RF,newData2, type="response")
        predict <- cbind(predict.class,predict.vote) 
        predict_table <- paste("randomForest/",basename,".ALL_new_species_predict_subRF.xls",sep="")
        write.table(predict,predict_table,sep = "\t" ,quote=F)
	}
}

##mds points
mds_points <- paste("randomForest/",basename,".ALL_new_species_subRF_pcoa_sites.xls",sep="")
write.table(sub.otu.mds$points,mds_points,sep="\t",quote=F)

##proximity table
#proximity <- paste("randomForest/",basename,".ALL_new_species_subRF_proximity_table.xls",sep="")
#write.table(sub.RF$proximity,proximity,sep="\t",quote=F)

## importance table 
vimp_table <- paste("randomForest/",basename,".ALL_new_species_imptance_table.xls",sep="")
importance <- importance(otu.rf,scale=T)[,imp_type]
sort_importance <- sort(importance,decreasing=T)
sort_importance <- as.data.frame(sort_importance)
variable <- rownames(sort_importance)
out_importance<- cbind(variable,sort_importance)
write.table(out_importance,vimp_table,sep="\t",quote=F,row.names=F)

## top importance species table
top_vimp <- paste("randomForest/",basename,".ALL_new_species_top",choose_top,"_vimp.xls",sep="")
imp <- importance(otu.rf)

top <- imp[order(imp[,imp_type],decreasing=T),][1:choose_top,]
write.table(t(otu)[rownames(top),],top_vimp,sep="	",quote=F)
	
