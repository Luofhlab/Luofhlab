
library(vegan)
#library(philentropy)
library(ggpubr)
library(reshape2)
library(patchwork)


dist <- read.delim("/mnt/ilustre/users/chun.luo/newmdt/cooeration/FD_GDM/result/adv2/new_analaysis_2020331/Pcoa/beta/bray_curtis_tax.txt",sep="\t",head=T,comment.char = "",check.names = FALSE, row.names = 1)
adist<-as.dist(dist)


options(stringsAsFactors=F)
sd <- read.delim("/mnt/ilustre/users/chun.luo/newmdt/cooeration/FD_GDM/result/adv2/new_analaysis_2020331/Pcoa/group.txt",head=T,sep="\t",comment.char = "",check.names = FALSE)

rownames(sd) <- as.character(sd[,1])
sd[,2] <- as.character(sd[,2])

dist <- dist[as.character(sd[,1]),][,as.character(sd[,1])]

pc_num <- as.numeric(unlist(strsplit("1-2","-")))
pc_x <- pc_num[1]
pc_y <- pc_num[2]

pca <- cmdscale(dist, k=3, eig=TRUE)
pc12 <- pca$points[,pc_num]
pc <- round(pca$eig/sum(pca$eig)*100,digits = 2)

pc12 <- as.data.frame(pc12)
colnames(pc12) <- c("pc_x","pc_y")
pc12['sample'] <- rownames(pc12)
colnames(sd)[1:2] <- c("sample","group")
sd$group<-factor(sd$group,levels=sd$group[!duplicated(sd$group)])
pc12 <- merge(pc12,sd,by="sample")

mex <- 0.2*abs(max(pc12$pc_x)-min(pc12$pc_x))
mey <- 0.2*abs(max(pc12$pc_y)-min(pc12$pc_y))

mytheme <- theme(panel.border = element_rect(color = "black",size = 1.0,fill = NA),
                 axis.text.x = element_blank(),
                # axis.text.y = element_blank(),
                 #axis.ticks = element_blank(),
                 text = element_text(size=18))
                 #plot.margin = unit(c(1, 0.5, 1, 1), "lines"))
pc12$group<-factor(pc12$group,levels=levels(sd$group))
p <- ggscatter(pc12, x = "pc_x", y = "pc_y",
        color = "group", shape = "group", palette = "npg", size=3)+
        ylab(paste0("PCoA",pc_y,"(",round(pc[pc_y],2),"%",")"))+
        xlab(paste0("PCoA",pc_x,"(",round(pc[pc_x],2),"%",")"))+
        geom_hline(yintercept = 0, color = '#B3B3B3', linetype = "longdash")+
        geom_vline(xintercept = 0, color = '#B3B3B3', linetype = "longdash")+
        theme(legend.position = "bottom",legend.title = element_blank())


p <- p +  stat_ellipse(aes(x = pc_x, y = pc_y, color = group), linetype = 1, level = 0.95)


p <- p + mytheme
p
if("mycol" %in% colnames(sd)){mycols<-sd$mycol[!duplicated(sd$mycol)]
p<-p+scale_color_manual(values=mycols)
}else{mycols=pal_npg("nrc")(10)[1:length(levels(sd$group))]}

ADONIS<-adonis(dist~sd$group)

TEST<-ADONIS$aov.tab$`Pr(>F)`[1]
R2adonis<-round(ADONIS$aov.tab$R2[1],digits = 3)
sink('adonis.txt')
print(ADONIS)
sink()

xpos<-ggplot_build(p)$layout$panel_scales_x[[1]]$range$range
ypos<-ggplot_build(p)$layout$panel_scales_y[[1]]$range$range

p+geom_text(aes(x=xpos[1],y=ypos[2]*1.1),label=paste("adnois R^2","=",R2adonis,'; ',"p=",TEST,sep = ''),size=6,hjust=0)
name <- paste0("/mnt/ilustre/users/chun.luo/newmdt/cooeration/FD_GDM/result/adv2/new_analaysis_2020331/Pcoa/pcoa_with_PC",pc_x,pc_y,"group.pdf")
ggsave(name,width=7,height=7)


cp <- combn(levels(pc12$group),2)
comp <- list()
for(i in 1:ncol(cp)){
          comp[[i]] <- cp[,i]
}



pr <- ggboxplot(pc12, x="group", y="pc_y", fill = "group", add = "jitter", palette = mycols) +
   stat_compare_means(comparisons = comp, label = "p.signif",method="wilcox.test")+
    theme(panel.border = element_rect(color = "black",size = 1.0,fill = NA),
                 axis.text.y = element_blank(),
                 axis.ticks.y= element_blank(),
                 axis.title.y = element_blank(),
                 legend.position = "none",
                 axis.text.x = element_text(size = 15,angle = 60,hjust = 1),
                 axis.title.x = element_text(size = 20))+theme(plot.margin = unit(c(0,0,0,0),'cm'))


#right

name <- paste0("/mnt/ilustre/users/chun.luo/newmdt/cooeration/FD_GDM/result/adv2/new_analaysis_2020331/Pcoa/pcoa_with_boxplot_PC",pc_y,".pdf")



myylimit=ggplot_build(pr)$layout$panel_scales_y[[1]]$range$range
p<-p+scale_y_continuous(limits = c(min(ypos,myylimit),max(ypos,myylimit)))
ypos<-ggplot_build(p)$layout$panel_scales_y[[1]]$limits[2]
p<-p+geom_text(aes(x=xpos[1],y=ypos),label=paste("adnois R^2","=",R2adonis,'; ',"p=",TEST,sep = ''),size=6,hjust=0)+
    theme(plot.margin = unit(c(0,0,0,0),'cm'))
if("mycol" %in% colnames(sd)){mycols<-sd$mycol[!duplicated(sd$mycol)]
p<-p+scale_color_manual(values=mycols)
pr<-pr+scale_color_manual(values=mycols)
}
library("gridExtra")
library("cowplot")
plot_grid(p, pr, ncol = 2, nrow = 1,rel_widths = c(2,1),align = 'h',axis = 'bt')
name <- paste0("/mnt/ilustre/users/chun.luo/newmdt/cooeration/FD_GDM/result/adv2/new_analaysis_2020331/Pcoa/pcoa_with_PC",pc_x,pc_y,".rightbox.group.pdf")
ggsave(name,width=10,height=7)


pt <- ggboxplot(pc12, x="group", y="pc_x", fill = "group", add = "jitter", palette = mycols) + coord_flip() +
   stat_compare_means(comparisons = comp, label = "p.signif",method="wilcox.test") +
   scale_x_discrete(limits = rev(levels(pc12$group)))+
    theme(panel.border = element_rect(color = "black",size = 1.0,fill = NA),
                 axis.text.x = element_blank(),
                 axis.ticks.x = element_blank(),
                 axis.title.x = element_blank(),
                 legend.position = "none",
                 axis.text.y = element_text(size = 15, angle = 0),
                 axis.title.y = element_text(size = 20))+theme(plot.margin = unit(c(0,0,0,0),'cm'))


write.table(pca$points, "/mnt/ilustre/users/chun.luo/newmdt/cooeration/FD_GDM/result/adv2/new_analaysis_2020331/Pcoa/pcoa_sites.xls", sep="\t", col.names=NA)
write.table(pca$eig/sum(pca$eig), "/mnt/ilustre/users/chun.luo/newmdt/cooeration/FD_GDM/result/adv2/new_analaysis_2020331/Pcoa/pcoa_importance.xls", sep="\t")

