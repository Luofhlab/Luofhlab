
summarySE <- function(data=NULL, measurevar, groupvars=NULL, na.rm=FALSE,conf.interval=.95, .drop=TRUE) 
{
	library(plyr)
	
	# New version of length which can handle NA's: if na.rm==T, don't count them
	length2 <- function (x, na.rm=FALSE) {
		if (na.rm) sum(!is.na(x))
		else       length(x)
	}
	
	# This does the summary. For each group's data frame, return a vector with
	# N, mean, and sd
	datac <- ddply(data, groupvars, .drop=.drop,
								 .fun = function(xx, col) {
									 c(N    = length2(xx[[col]], na.rm=na.rm),
										 mean = mean   (xx[[col]], na.rm=na.rm),
										 sd   = sd     (xx[[col]], na.rm=na.rm)
									 )
								 },
								 measurevar
	)
	
	# Rename the "mean" column    
	datac <- rename(datac, c("mean" = measurevar))
	
	datac$se <- datac$sd / sqrt(datac$N)  # Calculate standard error of the mean
	
	# Confidence interval multiplier for standard error
	# Calculate t-statistic for confidence interval: 
	# e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
	ciMult <- qt(conf.interval/2 + .5, datac$N-1)
	datac$ci <- datac$se * ciMult
	
	return(datac)
}

cbPalette <- c(  "#0000C0", "#A00000", "#0072B2", "#D55E00", "#CC79A7")
data0 =read.table("wilcox_genus.filtered.xls",sep="\t",head=T,check.names = F)
group=read.table("group.txt",sep="\t")
dname=colnames(data0)

rownames(data0)<-data0[,1]
data0 <-data0[,-1]

rownames(group) <-group[,1]

data=data0
scal=F
if(scal=="T"){
	data <-apply(data0,2,function(x) as.numeric(x)/sum(as.numeric(x))) 
	rownames(data)<-rownames(data0)	
}

rowsum <-sapply(1:nrow(data),function(x) sum(data[x,]))

top=0
if(top==0 || top > nrow(data)){
	top=nrow(data)
}
data<-data[order(rowsum,decreasing=TRUE),][1:top,]

library(ggplot2)
library(reshape2)
d=melt(t(data),measure.vars=colnames(data))
colnames(d)=c("sample","x","abundance")
colnames(group)=c("sample","group")
gda <- merge(d,group,by="sample")

sse=summarySE(gda, measurevar="abundance", groupvars=c("x","group"))
sse$group <- factor(sse$group, levels =c('NC', 'T1D') )
#pdf("wilcox_genus.filtered.xls.pdf",width=7,height=6)
ggplot(sse, aes(x=factor(x,levels=rownames(data)), y=abundance, fill=group)) + 
	geom_bar(position=position_dodge(), stat="identity") +
	ylab("Relative Abundance (%)") +
	xlab("") +
	theme(axis.text.x = element_text(angle = 70, hjust = 1, colour = "gray10"),
				panel.background=element_rect(color="transparent"))+
	geom_errorbar(aes(ymin=abundance-se, ymax=abundance+se),
								width=.2,# Width of the error bars
								position=position_dodge(.9))+
	scale_fill_manual(values=cbPalette)+guides(fill=guide_legend(title=NULL))
dev.off()

width=length(unique(group[,2]))*nrow(data)*0.05+5


ggplot(sse, aes(x=factor(x,levels=rownames(data)), y=abundance, fill=group)) + 
        geom_bar(position=position_dodge(), stat="identity") +
        ylab("Relative Abundance (%)") +
        xlab("") +
        theme(axis.text.x = element_text(angle = 70, hjust = 1, colour = "gray10"),
                                panel.background=element_rect(color="transparent"))+
        geom_errorbar(aes(ymin=abundance-se, ymax=abundance+se),
                                                                width=.2,# Width of the error bars
                                                                position=position_dodge(.9))+
        scale_fill_manual(values=cbPalette)+guides(fill=guide_legend(title=NULL))
	ggsave("wilcox_genus.filtered.xls.pdf",width=width, height=7,dpi=300)
	
