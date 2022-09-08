                                                                         
rtr <-1
ctr <- 1
rs <-0
cs <-1
cramp <-"darkblue-darkgreen-yellow-darkred"
colramp <-unlist(strsplit(cramp,"-"))
marble <-"2-0-0-5"
marble <-as.numeric(unlist(strsplit(marble,"-")))
ctr_lab <-0
rtr_lab <-0

ctop <- 0
rtop <- 50
logo <- "T"
tran <-0

mycol <- c(24,34, 51, 142, 23, 50, 27, 31, 75, 525, 62, 119, 46, 475, 554, 622, 483, 657, 545, 402, 477, 503, 40, 115, 5, 376,473,546,482)
#mycol <-c(24,119,132,147,454,89,404,123,529,463,104,552,28,54,84,256,100,558,43,652,31,610,477,588,99,81,503,562,76,96,495)
mycol <-colors()[rep(mycol,20)]

otu <-read.table(file="genus.xls",header=T,check.names=FALSE,sep="\t",fill=T)
rownames(otu) <- otu[,1]
rownames(otu) <-sapply(rownames(otu),function(x) gsub("_*{.+}","",x,perl = TRUE)) 
otu <-otu[,-1]
if(tran==1){ otu <- t(otu) }

rgs <-"none"
if(rgs!="none"){
      group <- read.table("none")
      glst <- lapply(1:length(unique(group[,2])),function(x)group[which(group[,2] %in% unique(group[,2])[x]),1])
      names(glst) <-unique(group[,2])
      tab <-sapply(1:length(glst),function(x) apply(otu[as.character(as.vector(glst[[x]]))],2,sum))
      otu <-tab[apply(tab,1,function(x)any(x>0)),]      
      rownames(otu) <-unique(group[,2])
}

cgs <-"map"
if(cgs!="none"){
      group <- read.table("map")
      glst <- lapply(1:length(unique(group[,2])),function(x)group[which(group[,2] %in% unique(group[,2])[x]),1])
      names(glst) <-unique(group[,2])
      tab <-sapply(1:length(glst),function(x) apply(otu[as.character(as.vector(glst[[x]]))],1,sum))
      otu <-tab[apply(tab,2,function(x)any(x>0)),]           
	  colnames(otu) <-unique(group[,2])
}

### row design
rd <-"none"
if(rd!="none"){
			de <-read.table(rd,sep="\t",fill=T)
			de <-de[order(de[,2]),]
			otu <-otu[as.vector(de[,1]),]  # otu sort as de
			rde <-de[,2]
			names(rde)<-de[,1]
			#rde <- as.vector(rde[rownames(otu_hmp)])  # de sort as otu
			rde_leve <-as.matrix(sort(table(rde),decreasing=T))  
			rde_leve <-as.matrix(table(rde))
			rde_leve <-cbind(rde_leve,2:(length(rde_leve)+1))
			rde_legend <-rde_leve 
			rdesign=TRUE
}else{
		rde="none"
		rde_leve="none"
		rde_legend="none"
		rdesign=FALSE
}

### column design 
cd <-"none"
if(cd!="none"){
			de <-read.table(cd,sep="\t",fill=T)
			de <-de[order(de[,2]),]
			otu <-otu[,as.vector(de[,1])] # otu sort as de
			cde <-de[,2]
			names(cde)<-de[,1]
			#cde <- as.vector(cde[colnames(otu_hmp)])   # de sort as otu
			cde_leve <-as.matrix(sort(table(cde),decreasing=T))
			cde_leve <-cbind(cde_leve,2:(length(cde_leve)+1))
			cde_legend <-cde_leve 
			cdesign=TRUE
}else{
		cde="none"
		cde_leve="none"
		cde_legend="none"
		cdesign=FALSE
}


otu <-otu[apply(otu,1,function(x)any(x>0)),] 
otu <-otu[,apply(otu,2,function(x)any(x>0))] 

al <- which(rownames(otu) %in% c("All"))
if(length(al)){ otu <-otu[-al,] }


if(rtop >0){
      rsum <-sapply(1:nrow(otu),function(x) sum(otu[x,]))
      otu<-otu[order(rsum,decreasing=TRUE),]
      if(50<=nrow(otu)){
      otu<-otu[1:rtop,]}
}

if(ctop >0){
      csum <-sapply(1:ncol(otu),function(x) sum(otu[,x]))
      otu<-otu[,order(csum,decreasing=TRUE)]
      if(0<=ncol(otu)){
      otu<-otu[,1:ctop]}
}

#print(otu) 

otu_hmp <-otu
if(cs>0){
		otu_hmp <-apply(otu,2,function(x) x/sum(x)*cs) 
}
if(rs>0){
	  otu_hmp <-t(apply(otu,1,function(x) x/sum(x)*rs) )
}
colnames(otu_hmp) <-colnames(otu)
rownames(otu_hmp) <-rownames(otu)
mo <-max(otu_hmp)

#print(otu_hmp)


####### make hclust tree ######## 
if(rtr==1){
         library(vegan)
         dat.bray <- vegdist(otu_hmp,method="bray")
         #write.table(file="dat.bray.dist.xls",as.matrix(dat.bray),sep="	")
         #dput(dat.bray,"dat.bray.xls")
         hr <-hclust(dat.bray,method="complete")
         rowtree=as.dendrogram(hr)
         ord_row <- hr$order
}else{
		ord_row <-c(nrow(otu_hmp):1)
}

if(ctr==1){
   library(vegan)
   dat.t.bray <-vegdist(t(otu_hmp),method="bray")
   #write.table(file="dat.t.bray.dist.xls",as.matrix(dat.t.bray),sep="	")
   hc <-hclust(dat.t.bray,method="complete")
   coltree=as.dendrogram(hc)
   ord_col <- hc$order
}else{
   ord_col <-c(1:ncol(otu_hmp))
}

####### read in a Newick  Tree ########
readtre <-function(tre,cor,hmp,design=FALSE,de_leve="none",de="none",short=0){
		library(ape)
		source("/mnt/ilustre/users/meta/Scripts/Diversity/meta_pipe/bin/plot.phylo.r")
		#ord_row <- c()
		ord_lab <-c()
		newtree <-read.tree(file=tre)
			   if(cor=="row") labs <- rownames(hmp)
			   if(cor=="col") labs <- colnames(hmp)
			   nlab <- which(newtree$tip.label %in% labs )
			   tnlab <-which(newtree$tip.label %in% setdiff(newtree$tip.label,labs))
		if(length(tnlab)>0){
			   tnedge <-which(newtree$edge[,2] %in% tnlab)
			   eddge <- newtree$edge
			   tchop <-tnedge
			   s1 <-0
			   s2 <-length(tchop)
			   while(s2-s1>0){
			   s1 <-length(tchop)
			   chop <- c(tnlab,names(which(table(eddge[tchop,1])==2)))
			   tchop <- which(eddge[,2] %in% chop)
			   s2 <-length(tchop)
			   }
			   newtree$edge <-newtree$edge[-tchop,]
			   newtree$edge.length <-newtree$edge.length[-tchop] 
			   lastnlabs <-which(newtree$edge[,2]<=length(newtree$tip.label))
			   newtree$edge[lastnlabs,2] <- sapply(newtree$edge[lastnlabs,2],function(x) which(nlab %in% x)) 
			   newtree$tip.label <-newtree$tip.label[nlab]
			   newtree <-collapse.singles(newtree)
			   eg <-c(1:length(unique(newtree$edge[,1])))+length(nlab)
			   names(eg) <-unique(sort(newtree$edge[,1]))
			   newtree$edge[,1] <-as.vector(sapply(as.character(newtree$edge[,1]),function(x) eg[[x]]))
			   ed2 <- which(newtree$edge[,2]>length(newtree$tip.label))
			   newtree$edge[ed2,2] <-as.vector(sapply(as.character(newtree$edge[ed2,2]),function(x) eg[[x]]))
			   newtree$Nnode <-max(newtree$edge[,1])-min(newtree$edge[,1])+1
		}
		for(i in 1:length(newtree$tip.label)){
			if(cor=="row") ord_lab[i] <- which(rownames(hmp) %in% newtree$tip.label[i] )
			if(cor=="col") ord_lab[i] <- which(colnames(hmp) %in% newtree$tip.label[i] )  
		}
		#write.table(newtree$edge.length,"length.1")

		egcol <-c()
		if(design==TRUE){
			labcol <-vector("list")
			for (i in 1:length(newtree$tip.label)){
				if(cor=="row") labcol[[rownames(hmp)[i]]] <- de_leve[which(rownames(de_leve) %in% de[i]),2]
				if(cor=="col") labcol[[colnames(hmp)[i]]] <- de_leve[which(rownames(de_leve) %in% de[i]),2]
			}
			#mil <-min(newtree$edge.length)
			for(i in 1:(nrow(newtree$edge))){
				  n <-newtree$edge[i,2]
				  if(n <=length(newtree$tip.label)){
						egcol[i]=labcol[[newtree$tip.label[n]]]					 
				  }else{
						egcol[i]= 1
				  }
			}			
		}else{
			   egcol=1
		}

		for(i in 1:(nrow(newtree$edge))){
					mal <-max(newtree$edge.length)
					if(short>=1){
						 newtree$edge.length[i] <-newtree$edge.length[i]+(short-1)*mal 
					}else if(short>0){ 
						 newtree$edge.length[i] <-newtree$edge.length[i]*short
					} 
		}
		ntr <- list(newtree=newtree,ord_lab=ord_lab,egcol=egcol)
}


if(rtr==2){	
		source("/mnt/ilustre/users/meta/Scripts/Diversity/meta_pipe/bin/plot.phylo.r")
		ntre <-readtre(tre="none",cor="row",hmp=otu_hmp,design=rdesign,de_leve=rde_leve,de=rde,short=0)
		rowtree <-ntre$newtree
		ord_row <-ntre$ord_lab
		regcol <-ntre$egcol
}
if(ctr==2){	
		source("/mnt/ilustre/users/meta/Scripts/Diversity/meta_pipe/bin/plot.phylo.r")
		ntre <-readtre(tre="none",cor="col",hmp=otu_hmp,design=cdesign,de_leve=cde_leve,de=cde,short=0)
		coltree <-ntre$newtree
		ord_col <-ntre$ord_lab
		cegcol <-ntre$egcol		
}

########################## set order and chage value by log && heat col#########################
otu_hmp <-otu_hmp[ord_row,ord_col] 
nr <- dim(otu_hmp)[1]
nc <- dim(otu_hmp)[2]
rowlab <-rownames(otu_hmp)
collab <-colnames(otu_hmp)
#print(otu_hmp)
otu_hmp_tmp <- cbind(row.names(otu_hmp),otu_hmp)
colnames(otu_hmp_tmp) <- c("Taxon",colnames(otu_hmp))
write.table(otu_hmp_tmp,row.names=FALSE,"heatmap_genus_top50.pdf.xls",sep="	",eol="
",quote=FALSE)
#library(gplots)
kbn <-200
brks <-vector()
heatcol <-vector()
if(logo=="T"){			
			x <-otu_hmp 
			x[x==0]=min(x[x!=0])*0.1
			otu_hmp <- log10(x)			
}

dmin <- min(otu_hmp)
dmax <-max(otu_hmp)
brks <- c(seq(dmin,dmax,length.out=kbn+1))	

		ramp <-colorRamp(as.vector(colramp))
		heatcol <- paste(rgb(ramp(seq(0,1,length=kbn)),max=255),"FF",sep="")

#print(otu_hmp)
########################## plot heatmap #########################

pdf0 <-"heatmap_genus_top50.pdf"
pdf(pdf0,width=7,height=10)
op <- par(no.readonly = TRUE)
on.exit(par(op))

h1=0.01;h2=0.01;h3=0.85;h4=0.15
w1=0.01;w2=0.01;w3=0.01;w4=0.97

if(ctr==1||ctr==2){h1=0.12}
if(cd!="none" && ctr !=2){h2=0.02}
if(rtr==1||rtr==2){w2=0.2;w4=0.7;}
if(rd!="none"){w1=0.3;}
if(rd!="none"&& rtr !=2){w3=0.05;}
lay_w <-c(w1,w2,w3,w4)*7
lay_h <-c(h1,h2,h3,h4)*10

l_w <-unlist(strsplit("n",":"))
l_h <-unlist(strsplit("n",":"))
if(l_w[1]!="n"){ lay_w <-as.numeric(l_w)*7 }
if(l_h[1]!="n"){ lay_h <-as.numeric(l_h)*10 }
layout(matrix(c(8,8,7,7,8,8,4,9,8,8,5,9,3,6,1,2),4,4),width=lay_w,height=lay_h, respect = FALSE)

  #######   
 # 8 8 8 3 #
 # 8 8 8 6 #
 # 7 4 5 1 # 
 # 7 9 9 2 # 
  ####### 
  
######## 1. heat image ######
mar_d <-marble[1]+1
mar_l <-marble[2]
mar_t <-marble[3]
mar_r <-marble[4]+13

par(mar = c(mar_d, mar_l, mar_t, mar_r))
plot(1, type = "n",xlim = c(0.5, nc+0.5), ylim = c(0.5, nr+0.5) , ann = FALSE,axes = FALSE,xaxs="i",yaxs="i")

#print (heatcol)
image(1:nc, 1:nr, t(otu_hmp), axes = FALSE, xlab = "", ylab = "", col =heatcol,add=TRUE,breaks =brks)

#grid(col = "lightgray",lty=1,nx=nc,ny=nr,lwd=0.1)
box()

clas=1
if(marble[4]>0) mtext(rowlab,side =4,at=c(1:nr),cex=0.6,las=1,line=0.5,font=3) 
if(marble[1]>0) mtext(collab,side =1,at=c(1:nc),cex=1,las=clas,line=0.7) 
if(marble[2]>0) mtext(rowlab,side =2,at=c(1:nr),cex=0.6,las=1,line=0.5,font=3) 
if(marble[3]>0) mtext(collab,side =3,at=c(1:nc),cex=1,las=clas,line=0.7) 

### 2. plot color key ###
par(mar = c(5, mar_l, 1, mar_r))
labl <-0
at <-seq(0,kbn,length.out=5)
if(rs==1||cs==1){ 
	xlab="Relative abundance of community (%)"
}else{
	xlab="Abundance of community"
}

image(c(0:kbn),0:1,z=matrix(c(1:kbn),kbn,1),xlim=c(0,kbn),col=heatcol,yaxt="n",xlab=xlab,ylab="",axes=FALSE,add=FALSE,cex.lab=1*1.5)
labl <-brks[at+1]
if(logo=="T"){
      labl <-10^c(-Inf,brks[at[-1]])
}
box()
if(cs==0&rs==0){
	labl<-round(labl,digits=0)
	dig=0
}else{
	labl<-round(labl*100,digits=2)
	mo <-mo*100
	dig=2
}

axis(side=1,at=at[-(length(at))],labels=labl[-(length(at))],pos=c(0,0),cex.axis=1*1.5)
axis(side=1,at=kbn,labels=round(mo,digits=dig),pos=c(0,0),cex.axis=1*1.5)

### 3. plot coltree ###
cstl <-FALSE
leaflab = "none"
if(ctr_lab>0) { leaflab="perpendicular" 
#	if(clas==3){leaflab="perpendicular" 
#	}else{leaflab="textlike"}	
	cstl=TRUE 
}
if(ctr==0){
	par(mar = c(0, 0, 0, 0))
	plot.new()
}else if(ctr==1){
			par(mar = c(ctr_lab, mar_l, 2, mar_r))
			plot(coltree,xlim = c(0.5, nc+0.5), ann = FALSE,axes = FALSE, leaflab = leaflab,xaxs="i",yaxs="i")
			#plot.phylo.yg(coltree,edge.width=0.8,cex=0.8,y.lim=c(0.5,nr+0.5),magic=TRUE,show.tip.label = cstl,xaxs="i",yaxs="i",direction="downwards")
}else if(ctr==2){
			par(mar = c(0, mar_l, 2, mar_r))
			plot.phylo.yg(coltree,edge.color=mycol[cegcol],edge.width=0.8,cex=0.8,x.lim=c(0.5,nc+0.5),magic=TRUE,show.tip.label = cstl,xaxs="i",yaxs="i",direction="downwards",label.offset=0.02)
}


### 4. plot rowtree ###
par(mar = c(mar_d, 0, mar_t, rtr_lab))
rstl <-FALSE
leaflab = "none"
if(rtr_lab>0) {leaflab="perpendicular";rstl=TRUE }
if(rtr==0){
	par(mar = c(0, 0, 0, 0))
	plot.new()
}else if(rtr==1){
			plot(rowtree,ylim = c(0.5, nr+0.5), ann = FALSE,axes = FALSE,col=c(1,2,3), leaflab = leaflab,xaxs="i",yaxs="i", horiz = TRUE)
			#plot.phylo.yg(rowtree,edge.width=0.8,cex=0.8,y.lim=c(0.5,nr+0.5),magic=TRUE,show.tip.label = rstl,xaxs="i",yaxs="i")
}else if(rtr==2){
			par(mar = c(mar_d, 0, mar_t, 0))
			plot.phylo.yg(rowtree,edge.color=mycol[regcol],edge.width=0.8,cex=rtr_lab,y.lim=c(0.5,nr+0.5),magic=TRUE,show.tip.label = rstl,xaxs="i",yaxs="i",label.offset=0.02)
}

### 5. plot row design bar
#par(mar = c(mar_d, 0, mar_t, 0))
if(rd!="none" && rtr !=2){
		 par(mar = c(mar_d, 0.1, mar_t, 0.2))
		 rde <- rde[ord_row]
         lcol<- as.vector(sapply(1:length(rde),function(i) mycol[rde_leve[which(rownames(rde_leve) %in% rde[i]),2]]))
         plot(x=0,type="n",xlim=c(0,1),ylim=c(0,nr),ann=FALSE,xaxs="i",yaxs="i",axes=FALSE)
         rect(rep(0,nr),seq(0,nr-1,1),rep(1,nr),seq(1,nr,1),col=lcol,border = NA)
         #box() 	
}else{
par(mar = c(0, 0, 0, 0))
plot.new()
}

### 6. plot column design bar
cd <-"none"
#par(mar = c(0, mar_l, 0.1, mar_r))
if(cd!="none" &&  ctr !=2){		 
		 par(mar = c(0.2, mar_l, 0.3, mar_r))
		 cde <- cde[ord_col]
         lcol<- as.vector(sapply(1:length(cde),function(i) mycol[cde_leve[which(rownames(cde_leve) %in% cde[i]),2]]))
         plot(x=0,type="n",xlim=c(0,nc),ylim=c(0,1),ann=FALSE,xaxs="i",yaxs="i",axes=FALSE)
         rect(seq(0,nc-1,1),rep(0,nc),seq(1,nc,1),rep(1,nc),col=lcol,border = NA)
         #box() 	
}else{
par(mar = c(0, 0, 0, 0))
plot.new()
}


### 7. row design bar legend
par(mar = c(0, 0, 0, 0))
if(rd!="none"){
		par(mar = c(1, 2, 0, 0))
		plot.new()
		n0 <- which(rownames(rde_legend) %in% " ")
		if(length(n0)){ rownames(rde_legend)[n0] <-"Unclassified"}
		ncol <-1
		if(length(rde_legend)>100){ ncol <-2}
		legend("topright",legend=rownames(rde_legend),col=mycol[rde_leve[,2]],fill=mycol[rde_leve[,2]],ncol=ncol,cex=1.2,bty="n")
}else{
par(mar = c(0, 0, 0, 0))
plot.new()
}



### 8. column design bar legend
if(cd!="none"){
		plot.new()
		par(mar = c(0, 1, 0, 0))
		n0 <- which(rownames(cde_legend) %in% " ")
		if(length(n0)){ rownames(cde_legend)[n0] <-"Unclassified"}
		ncol <-1
		if(length(cde_legend)>100){ ncol <-2}
		
		legend("bottomright",legend=rownames(cde_legend),col=mycol[cde_leve[,2]],fill=mycol[cde_leve[,2]],ncol=ncol,cex=1.2,bty="n")
}

dev.off()
