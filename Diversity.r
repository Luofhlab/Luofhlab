
options(stringsAsFactors = F)
#library(plyr)
library(ggpubr)
library(ggplot2)
library(tidyr)
library(scales)
library(RColorBrewer)
data_summary <- function(data, varname, groupnames){
  require(plyr)
  summary_func <- function(x, col){
    c(mean = mean(x[[col]], na.rm=TRUE),
      sd = sd(x[[col]], na.rm=TRUE))
  }
  data_sum<-ddply(data, groupnames, .fun=summary_func,
                  varname)
  data_sum <- rename(data_sum, c("mean" = varname))
  return(data_sum)
}
library(dplyr)
i_data <- "diversity.index.txt"
o_file <- "./"
g_design <- "group.txt"
pvalue<-0.05
method<-"wilcox.test"
df<-read.delim(i_data,head=T,sep="	",check.names=F)
colnames(df)[1]<-'samples'
group<-read.delim(g_design,sep="	",header=T)
colnames(group)[1:2]<-c("samples","group")
mylevels<-group$group[!duplicated(group$group)]
group$group<-factor(group$group,levels = mylevels)
if(!'mycol' %in% colnames(group)){
  mycol<-brewer.pal(n=length(mylevels),name='Paired')
}else{mycol<-group$mycol[!duplicated(group$mycol)]}
df1<-merge(df,group,by='samples')
#method='t.test'#method can be  wilcox.test,t.test, wilcox.test,anova, kruskal.test #
## color
#library(ggsci)
#mycol<-pal_gsea("default")(12)[c(12:9,1:4)]
#mycol<-brewer.pal(n=length(levels(as.factor(group$group))),name='Paired')
for(i in colnames(df)[-1]){
  #i='shannon'
df2<-data_summary(df1,i,'group')
write.table(df2,paste(o_file,i,'mean_sd.xls',sep=''),quote = F,col.names = T,row.names = F,sep = "	")
df3 <-as.data.frame( df1 %>% select(samples,i,group))
colnames(df3)[2]='i'
T.test<-as.data.frame(compare_means(i~group, data=df3, method = method, paired = FALSE))#paired only used for t.test and wilcox.test
#as.data.frame(compare_means(shannon~group, data=df3, method = 'anova', paired = FALSE))
overalltest<-as.data.frame(compare_means(i~group, data=df3, method = 'kruskal.test', paired = FALSE))
write.table(overalltest,paste(o_file,i,'.kruskal.test','.xls',sep=''),quote = F,col.names = T,row.names = F,sep = "	")
#levels(as.factor(T.test$group1))
colnames(T.test)[1]<-'type'
T.test$type<-i
#df1$type<-i
write.table(T.test,paste(o_file,i,method,'.xls',sep=''),quote = F,col.names = T,row.names = F,sep = "	")
x<-as.matrix(T.test[T.test$p<pvalue,][,c(2,3)])
nosig<-F
if(nrow(x)==0){
nosig<-T
x<-as.matrix(T.test[T.test$p<=1,][,c(2,3)])}
my_comparisons<-lapply(1:nrow(x),function(y) as.vector(x[y,]))
for_label=length(my_comparisons)
colnames(df3)[2]=i
for_y=max(df3[,2])
for_labely=NULL
for(j in 1:for_label){for_labely=c(for_labely,for_y*0.9+j*for_y*1.5/20)}

width=length(levels(as.factor(group$group)))
if(width<5){width=5}
height=length(my_comparisons)*0.5+5
ggboxplot(df3,x = "group", y = i,color = "group", palette = mycol,legend="none",add='jitter',add.params=list(size=3,jitter=0.1,alpha=0.5),outlier.shape = NA)+
  stat_compare_means(method = "kruskal.test",label.y = max(for_labely)*1.6)+
  stat_compare_means(comparisons = my_comparisons,
                     method = method,label = "p.signif",
                     label.y = for_labely*1.5)+
  #scale_y_continuous(expand = c(0, 0),  limits =c(0,for_y*1.5+for_label*for_y/10),oob = rescale_none)+
  theme(panel.grid = element_line(color = NA),
        panel.grid.minor = element_line(color = NA),
        panel.border = element_rect(fill = NA, colour = NA),
       axis.text.x  = element_text(size=10, colour="black", face = "bold",hjust = 1,angle=60),
        axis.title.x = element_blank(),
        axis.text.y  = element_text(size=10, colour="black", face = "bold",hjust = 0.5),
        axis.title.y = element_text(vjust=0.2, size = 12, face = "bold"))
ggsave(paste(o_file,i,"_sig_boxplot.pdf",sep=''),width = width/2+1.5,height = height/2.2+1.5)

if(nosig){
height=8
ggboxplot(df3,x = "group", y = i,color = "group", palette = mycol,legend="none",add='jitter',add.params= list(size=3,jitter=0.1,alpha=0.5),outlier.shape = NA)+
  stat_compare_means(method = "kruskal.test")+
  #scale_y_continuous(expand = c(0, 0),  limits =c(0,for_y*1.5),oob = rescale_none)+
  theme(panel.grid = element_line(color = NA),
        panel.grid.minor = element_line(color = NA),
        panel.border = element_rect(fill = NA, colour = NA),
        axis.text.x  = element_text(size=10, colour="black", face = "bold",hjust = 1,angle=60),
        axis.title.x = element_blank(),#element_text(vjust=0.1, face = "bold"),
        axis.text.y  = element_text(size=10, colour="black", face = "bold",hjust = 0.5),
        axis.title.y = element_text(vjust=0.2, size = 12, face = "bold"))
ggsave(paste(o_file,i,"_boxplot.pdf",sep=''),width = width/2+1.5,height = height/2.2+1.5)
}
}
