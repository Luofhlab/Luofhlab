#psych包用于计算相关性、p值等信息
library(psych)
#pheatmap包用于绘制相关性热图
library(pheatmap)
#reshape2包用于输出数据的整合处理
library(reshape2)
#读取微生物丰度信息表
#表头需带有分类水平、物种名称等关键信息
#第一列为样本名称信息
phy <-read.table(file = "phy.xls", sep = "\t", header = T,row.names=1)
#读取代谢物信息表
met <-read.table(file = "met.xls", sep = "\t", header = T,row.names=1)
#计算相关性矩阵（可选：”pearson”、”spearman”、”kendall”相关系数）、p值矩阵
cor <-corr.test(phy, met, method = "spearman",adjust="none")
#提取相关性、p值
cmt <-cor$r
pmt <- cor$p
head(cmt)
head(pmt)
#输出相关系数表格,第一行为代谢物信息，第一列为物种信息
cmt.out<-cbind(rownames(cmt),cmt)
write.table(cmt.out,file="cor.txt",sep="\t",row.names=F)
#输出p值表格，第一行为代谢物信息，第一列为物种信息
pmt.out<-cbind(rownames(pmt),pmt)
write.table(pmt.out,file="pvalue.txt",sep="\t",row.names=F)
#以关系对的形式输出表格
#第一列为物种名，第二列为代谢物名，第三、第四列对应显示相关系数与p值
df <-melt(cmt,value.name="cor")
df$pvalue <-as.vector(pmt)
head(df)
write.table(df,file="cor-p.txt",sep="\t")
#对所有p值进行判断，p<0.01的以“**”标注，p值0.01<p<0.05的以“*”标注
if (!is.null(pmt)){
ssmt <- pmt< 0.01
pmt[ssmt] <-'**'
smt <- pmt >0.01& pmt <0.05
pmt[smt] <- '*'
pmt[!ssmt&!smt]<- ''
} else {
  pmt <- F
 }
#自定义颜色范围
mycol<-colorRampPalette(c("blue","white","tomato"))(800)
#绘制热图,可根据个人需求调整对应参数
#scale=”none” 不对数据进行均一化处理 可选"row", "column"对行、列数据进行均一化
#cluster_row/col=T 对行或列数据进行聚类处理，可选F为不聚类
#border=NA 各自边框是否显示、颜色，可选“white”等增加边框颜色
#number_color=”white” 格子填入的显著性标记颜色
#cellwidth/height=12 格子宽度、高度信息
 
pheatmap(cmt,scale = "none",cluster_row = T, cluster_col = T, border=NA,
      display_numbers = pmt,fontsize_number = 12, number_color = "white",
      cellwidth = 20, cellheight =20,color=mycol)
#图片保存，代码中输入”filename=”,或在R语言软件中点击“文件-另存为” 进行保存
pheatmap(cmt,scale = "none",cluster_row = T, cluster_col = T, border=NA,
      display_numbers = pmt, fontsize_number = 12, number_color ="white",
      cellwidth = 20, cellheight =20,color=mycol，filename="heatmap.pdf")