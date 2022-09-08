
mycol <- c(34, 51, 142, 26, 31, 371, 36, 7, 12, 30, 84, 88, 116, 121, 77, 56, 386, 373, 423, 435, 438, 471, 512, 130, 52, 47, 6, 11, 43, 54, 367, 382, 422, 4, 8, 375, 124, 448, 419, 614, 401, 403, 613, 583, 652, 628, 633, 496, 638, 655, 132, 503, 24)
mycol <-colors()[rep(mycol,20)]
otu <-read.table(file="species.xls",header=T,check.names=FALSE,sep="\t")
rownames(otu) <- otu[,1]
rownames(otu) <-sapply(rownames(otu),function(x) gsub("_*{.+}"," ",x,perl = TRUE)) 
####
library(dplyr)
otu<-as.data.frame(otu %>% select(-1))
al <- which(rownames(otu) %in% c("All"))
if(length(al)) otu <-otu[-al,]
gs <-"ALL"

if(gs!="ALL"){
      group <- read.delim("ALL",header=T,check.names=F)
	  glst <- lapply(1:length(unique(group[,2])),function(x)group[which(group[,2] %in% unique(group[,2])[x]),1])
	  names(glst) <-as.character(unique(group[,2]))
	  tab <-sapply(1:length(glst),function(x) apply(otu[as.character(as.vector(glst[[x]]))],1,mean))
	  otu <-tab[apply(tab,1,function(x)any(x>0)),,drop=FALSE]      
	  colnames(otu) <-unique(group[,2])
	  rm(glst)
	  rm(tab)
		       }

n <--1
m <--1
p <-0.01
s <--1
od <-"d"
oth <-"T"
rowsum <-sapply(1:nrow(otu),function(x) sum(otu[x,],na.rm=T))
if(od =="d"){
	otu<-otu[order(rowsum,decreasing=TRUE),,drop=FALSE]
}else if(od =="i"){
	otu<-otu[order(rowsum,decreasing=FALSE),,drop=FALSE]
}

so <-"F"
if(so=="T"){
	otu<-otu[,order(colnames(otu),decreasing=FALSE),drop=FALSE]
}

if(n>0){
     #otu[order(otu[,1],decreasing=T)[1:10],1]
     max_names <- sapply(1:ncol(otu),function(x) rownames(otu)[order(otu[,x],decreasing=T)[1:n]])
     otu1 <-otu[which(rownames(otu) %in% unique(as.vector(max_names))),,drop=FALSE]
     otu2 <-otu[-which(rownames(otu) %in% unique(as.vector(max_names))),,drop=FALSE]
     other <- sapply(1:ncol(otu2),function(y) sum(otu2[,y,drop=FALSE],na.rm=T))
     otu<-rbind(otu1,other)
     rownames(otu)[nrow(otu)] <-"Others"	 
     #unique(as.vector(max_names))
     #use_list <-which(rownames(otu) %in% unique(as.vector(max_names)))
     #rownames(otu)[which(rownames(otu) %in% unique(as.vector(max_names)))]
    
}else if(m>0){
#        otu<-otu[order(rowsum,decreasing=TRUE),,drop=FALSE]
		otu1<-otu[1:m,,drop=FALSE]
		otu2<-otu[(m+1):nrow(otu),,drop=FALSE]
		other <- sapply(1:ncol(otu2),function(y) sum(otu2[,y],na.rm=T))
		otu<-rbind(otu1,other)
		  rownames(otu)[nrow(otu)] <-"Others"       

}else if(p >0){
	if(oth=="T"){
	 otu_pec <- otu
	 #otu_pec <- sapply(1:ncol(otu),function(x) otu_pec[,x] <-otu[,x]/sum(otu[,x],na.rm=T))
	 otu_pec<-sweep(otu,2,colSums(otu,na.rm=T),`/`)
	 minp <-sapply(1:nrow(otu_pec),function(y) all(otu_pec[y,]<=p)) 	 
	 otu_xp <-otu[minp,,drop=FALSE]
	 other <- sapply(1:ncol(otu_xp),function(y) sum(otu_xp[,y],na.rm=T))
	 otu <-rbind(na.omit(otu[!minp,,drop=FALSE]),other)
	 rownames(otu)[nrow(otu)] <-"Others"
	 rm(otu_pec)
	}else{
	 otu_pec <- otu
         otu_pec <- sapply(1:ncol(otu),function(x) otu_pec[,x] <-otu[,x]/sum(otu[,x],na.rm=T))
         minp <-sapply(1:nrow(otu_pec),function(y) all(otu_pec[y,]<=p))          
         otu_xp <-otu[minp,,drop=FALSE]
         other <- sapply(1:ncol(otu_xp),function(y) sum(otu_xp[,y],na.rm=T))
         otu <-rbind(otu[!minp,,drop=FALSE])
         rm(otu_pec)
	}
}else if(s >0){
	 minp <-sapply(1:nrow(otu),function(y) all(otu[y,]<=s)) 	 
	 otu_xp <-otu[minp,,drop=FALSE]
	 other <- sapply(1:ncol(otu_xp),function(y) sum(otu_xp[,y],na.rm=T))
	 otu <-rbind(otu[!minp,,drop=FALSE],other)
	 rownames(otu)[nrow(otu)] <-"Others"	 
	 rm(otu_xp)
}
if(ncol(otu)>1){
del <-unlist(sapply(1:ncol(otu),function(x) if(sum(otu[,x],na.rm=T)==0) x))
}else{del<-NULL}
if(!is.null(del)) {
  mydel <-colnames(otu)[c(del)]
  otu <-otu[,-c(del)]
}

bar <-"T"
pie <-"T"

horiz <-"F"
################# plot  bar     ########################################
if(bar =="T"){
	dat <-sapply(1:ncol(otu),function(x) otu[,x]/sum(otu[,x],na.rm=T))
	colnames(dat) <-colnames(otu)
	rownames(dat) <-rownames(otu)
	dat1 <-sapply(1:ncol(otu),function(x) otu[,x])
	colnames(dat1) <-colnames(otu)
	rownames(dat1) <-rownames(otu)        
	dat1_temp<-cbind(row.names(dat1),dat1)
	colnames(dat1_temp)<-c("Taxon",colnames(dat1))
	dat_temp<-cbind(row.names(dat),dat)
	colnames(dat_temp)<-c("Taxon",colnames(dat))
        groupname<-gsub('.*/','',"ALL")
	inputname<-gsub('.*/','',"species.xls")
	write.table(dat1_temp,row.names=FALSE,paste('./',groupname,'_new_',inputname,sep=''),sep="	",quote=FALSE)
        write.table(dat_temp,row.names=FALSE,paste('./',groupname,'_new.percent_',inputname,sep=''),sep="	",quote=FALSE)
	lab <-rownames(dat)
	str <-strwidth(lab, units = "inches")
	cnum <-ceiling(length(str)/3)
	nstr <- lapply(1:3,function(x) if(cnum*x<=length(str)){str[(cnum*(x-1)+1):(cnum*x)]}else{str[(cnum*(x-1)+1):length(str)]})
	nlab <- lapply(1:3,function(x) if(cnum*x<=length(lab)){lab[(cnum*(x-1)+1):(cnum*x)]}else{lab[(cnum*(x-1)+1):length(lab)]})
	mw <-unlist(lapply(nstr,max))
	mw[1] <-mw[1]*1.3
	lx <-"F"
  df<-as.data.frame(dat)
  
  width_bar<-log10(ncol(df)) *5
  width_legend<-log10(nrow(df))*5
  width_name<-0.12*ncol(df)
  width<-max(width_bar, width_legend,width_name)
  height_bar=3
  library(reshape2)
  df$tax<-rownames(df)
  tax.melt<-melt(df,id.vars='tax')
  colnames(tax.melt)<-c('tax','sample','abundance')
  tax.melt$tax<-factor(tax.melt$tax,levels = rownames(df))
  if('Others' %in%  tax.melt$tax ){tax.melt$tax <-relevel(tax.melt$tax,'Others')
                                    temporder<-levels(tax.melt$tax)
                                    temporder<-temporder[c(2:length(temporder),1)] 
                                    tax.melt$tax <-factor(tax.melt$tax,levels=temporder)
                                     }
  tax.melt$sample<-factor(tax.melt$sample,levels=colnames(df)[-ncol(df)])
  lnc<-round(width/(mean(nchar(rownames(df)))*0.15))-1
  height_legend<-nrow(df)/lnc*0.5
  height<-round(height_bar+height_legend)
  if(height/width>0.8 &&ncol(df)<=4){lp='right'}else{lp='bottom'}
  if(lp=='right'){
    lnc<-ceiling(nrow(df)/25)
    width=max(nchar(rownames(df))*0.07*lnc)+2
    #width=min(width,40)
    height=nrow(df)/lnc*0.15
    if(height<=3.5){height=3.5}
    }else{if(height/width>1){
  lnc=lnc+3
  width_legend<-max(nchar(rownames(df))*0.07*lnc)
  width<-max(width_bar, width_legend,width_name,6)
  height=nrow(df)/lnc*0.30+6
  }}
library(ggplot2)
barwidth=min(0.9,ncol(otu)/width)
if(lp=='right'){
if(lnc<0){lnc=1}
print(paste('height=',height,'width=',width,sep=' '))
ggplot(data=tax.melt, aes(x=sample, y=abundance, fill=tax)) +
  geom_bar(stat="identity",position = position_fill(reverse = TRUE),width=barwidth)+
  guides(fill = guide_legend(ncol=lnc,override.aes = list(size = 1)))+
  scale_fill_manual(values = mycol) +
  theme(legend.position=lp,legend.title = element_blank(),
        legend.direction="horizontal",legend.key.size = unit(0.5, "lines"),
        panel.background=element_blank(),
        panel.grid.major.x = element_line(colour = "white"),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.title.x=element_blank(),
        axis.text.y = element_text( colour="black"),
        axis.text.x=element_text(angle = 90,vjust = 0.5,hjust=1,colour="black"),
        axis.title.y = element_text(vjust=0.2, size = 12, face = "bold"),
        axis.line.y=element_line(),axis.ticks.x = element_blank()
  ) + scale_y_continuous(expand = c(0,0))+labs(y = "Relative abundance")
} else{
if(lnc<0){lnc=1}
ggplot(data=tax.melt, aes(x=sample, y=abundance, fill=tax)) +
 geom_bar(stat="identity",position = position_fill(reverse = TRUE),width=barwidth)+
  guides(fill = guide_legend(ncol=lnc,override.aes = list(size = 1)))+
  scale_fill_manual(values = mycol) +
  theme(legend.position=lp,legend.justification=c(0, 0),legend.title = element_blank(),
        legend.direction="horizontal",legend.key.size = unit(0.5, "lines"),
        panel.background=element_blank(),
        panel.grid.major.x = element_line(colour = "white"),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.title.x=element_blank(),
        axis.text.y = element_text( colour="black"),
        axis.text.x=element_text(angle = 90,vjust = 0.5,hjust=1,colour="black"),
        axis.title.y = element_text(vjust=0.2, size = 12, face = "bold"),
        axis.line.y=element_line(),axis.ticks.x = element_blank()
  ) + scale_y_continuous(expand = c(0,0))+labs(y = "Relative abundance")

}


ggsave(paste('./','bar.','ALL','.',inputname,'.pdf',sep=''),dpi = 300,width = width,height = height,limitsize = FALSE)
}

######## bar finished ####################
if(pie =="T"){

######## function spie ##########
spie <-function (x, labels = names(x), edges = 200, radius = 0.8, clockwise = FALSE, 
    init.angle = if (clockwise) 90 else 0, density = NULL, angle = 45, 
    col = NULL, border = NULL, lty = NULL, main = NULL, textlab=TRUE, legend = FALSE,...) 
{   
    if (!is.numeric(x) || any(is.na(x) | x < 0)) 
        stop("'x' values must be positive.")
    if (is.null(labels)) 
        labels <- as.character(seq_along(x))
    else labels <- as.graphicsAnnot(labels)

    if(legend) { layout(matrix(1:2,1,2) ) 
                 par1 <-par(mar=c(1,1,1,0))
                 par2 <-par(mar=c(1,0,4,1))
    }

    x <- c(0, cumsum(x)/sum(x,na.rm=T))
    dx <- diff(x)
    nx <- length(dx)
    plot.new()
    pin <- par("pin")
    xlim <- ylim <- c(-1, 1)
    if (pin[1L] > pin[2L]) 
        xlim <- (pin[1L]/pin[2L]) * xlim
    else ylim <- (pin[2L]/pin[1L]) * ylim
    vx<-0.7+0.3
	vy<-0.7+0.3
	ylim[2] <-ylim[2]*vy	
    if(legend) {v <- 0.8;par(par1)}
    plot.window(xlim*vx, ylim, "", asp = 1)
    if (is.null(col)) 
        col <- if (is.null(density)) 
            c("white", "lightblue", "mistyrose", "lightcyan", 
                "lavender", "cornsilk")
        else par("fg")
    col <- rep(col, length.out = nx)
    border <- rep(border, length.out = nx)
    lty <- rep(lty, length.out = nx)
    angle <- rep(angle, length.out = nx)
    density <- rep(density, length.out = nx)
    twopi <- if (clockwise) 
        -2 * pi
    else 2 * pi
    t2xy <- function(t) {
        t2p <- twopi * t + init.angle * pi/180
        list(x = radius * cos(t2p), y = radius * sin(t2p))
    }

    pl1 <- pl2 <- c(0,-radius*1.5)
    lb1 <- lb2 <-array()
    lb1[1] <- lb2[1] <- "lab0"
    li1 <- li2 <-1
    for (i in 1L:nx) {
        n <- max(2, floor(edges * dx[i]))
        P <- t2xy(seq.int(x[i], x[i + 1], length.out = n))
        polygon(c(P$x, 0), c(P$y, 0), density = density[i], angle = angle[i], 
            border = border[i], col = col[i], lty = lty[i])
        pm <- t2xy(mean(x[i + 0:1]))
        lab <- as.character(labels[i])
        if (!is.na(lab) && nzchar(lab)) {
            if(pm$x >=0){ 
                 l1 <-c(pm$x,pm$y)
                 pl1 <-rbind(pl1,l1)
                 li1 <-li1+1
                 lb1[li1] <-lab
            }else { 
                 l2 <-c(pm$x,pm$y)
                 pl2 <-rbind(pl2,l2)
                 li2 <-li2+1
                 lb2[li2] <-lab
            }
        }
    }
    row.names(pl1) <-lb1
    row.names(pl2) <-lb2


############### labelxy ####
    labelxy <- function(pl) {
         pl <-pl[order(pl[,2]),]
         by <-pl[1,2]
         bx <-pl[1,1]
         d1 <-1.3
         for(j in 2:nrow(pl)) {
             ply <- pl[j,2]
             plx <- pl[j,1]
             pmy <- 1.3 * ply
             pmx <- 1.3 * plx
             d2 <-1.3
             if(ply<0 & abs(plx)<0.3*radius){ 
                  if(d1>1.1) {d1 <-d1-0.04}    
					# while(abs(pmx-bx)<0.1 ){                   
                     # d2 <-d2+0.01
                     # pmx <- plx*d2}				  
					if(abs(pmx-bx)<0.1){
							if(pmx-bx>0){
									pmx=0.1+bx
							}else{
									pmx=-0.1+bx
							}
							d2<-pmx/plx
					}
             }else{ d1 <-1.2 }
			 if(pmy-by<0.08) {pmy <-0.08+by}
             #while(pmy-by<0.08){ pmy <- pmy+0.01 }
             d3 <-d2
			 if(pmy*pmy+pmx*pmx <=radius*radius){
				 if(pmx>0){
						pmx <- sqrt(radius*radius-pmy*pmy)
				 }else{
						pmx <- -sqrt(radius*radius-pmy*pmy)
				 }
				 d3<-pmx/plx
			 }
             # while(pmy*pmy+pmx*pmx <=radius*radius){
                  # d3 <-d3+0.01
                  # pmx <- plx*d3
             # } 
             
             lines(c(1, d1) * plx,c(1, d1) * ply) #line1                                                  
             lines(c(d1* plx, pmx) , c(d1*ply, pmy) ) #line2
             lines(c(pmx, pmx*1.03) , c(pmy,pmy ))  #line3
             text(1.036*pmx, pmy, rownames(pl)[j], xpd = TRUE, adj = ifelse(plx < 0, 1, 0), ...)
             by <-pmy
             bx <-pmx
         }
    }
###############

  if(textlab) {       
            labelxy(pl1)
            labelxy(pl2)
  }
  title(main = main, ...)
  if(legend) { plot.new()
               par(par2)
               legend("topleft",legend=labels,fill=col)

  }


  invisible(NULL)


}

#####################################################################

##### function ppie : plot a table of samples #########
ppie <-function(dat,label,col,smp){
#	tiff(paste("pie.",smp,".species.xls.tiff",sep=""),width=10,height=5,pointsize=15)
	#pdf(paste("pie.",smp,".species.xls.pdf",sep=""),width=10,height=5)
	pdf(paste('./','pie.',smp,'.',inputname,'.pdf',sep=''),width=10,height=5)
	label <-sapply(1:nrow(dat),function(x) paste(label[x]," ",round(dat[x,1]/sum(dat)*100,digits=2),"%",sep=""))
	spie(dat,border=NULL,labels=label,col=col,main=smp,cex=0.8,textlab=TRUE,legend=FALSE,clockwise=F,init.angle =0)
	dev.off()
}
###########################################

#########plot    pie             ##########
pcol=mycol[1:nrow(otu)]
dl <-lapply(1:ncol(otu),function(y) which(otu[,y]>0))
sapply(1:ncol(otu),function(y) ppie(dat=as.matrix(otu[dl[[y]],y]),label=rownames(otu)[dl[[y]]],col=pcol[dl[[y]]],smp=colnames(otu)[y]))
}


