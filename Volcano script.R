
#Adjust names of files/ lab titles/x & y limits/ Colours

library(readxl)
library(ggpubr)
VolcanoData=read_excel("Volcano_MII_YOUNG_AMA.xlsx")
colnames(VolcanoData)[1]="label"

VolcanoData$expression = ifelse(VolcanoData$BHcor <= 0.05 & abs(VolcanoData$log2Foldchange) > 0.58, 
                                ifelse(VolcanoData$log2Foldchange> 0.58 ,'High abundance in MII Young ','Low abundance in MII Young '),'Stable abundance')


library(ggplot2)


library(ggplot2)
p <- ggplot(data = VolcanoData, 
            aes(x = log2Foldchange, 
                y = -log10BH,  
                colour=expression,
                label = label)) +
  geom_point(alpha=0.4, size=4)+
  scale_color_manual(values=c("#e000fa", "green","grey"),name=NULL)+
  xlim(c(-3, 3)) +
  ylim(c(0, 4.5))+
  geom_vline(xintercept=c(-0.58,0.58),lty=4,col="black",lwd=0.8) +
  geom_hline(yintercept = 1.301029996,lty=4,col="black",lwd=0.8) +
  labs(x = expression("MII vs GV (Log"[2]*"FC)"),
      y = expression(-Log[10]("P.adj")),
       title="Young group")  +
  geom_text(label=VolcanoData$names, size=2,colour='black', hjust=1,vjust=0)+
  theme_bw()+
  theme(panel.grid = element_blank(),
        panel.border = element_rect(linewidth = 2,color = "black"),
        plot.title = element_text(size=25,hjust = 0.5,color = "black"),
        axis.title.x = element_text(size = 25,color = "black"), 
        axis.text.x = element_text(size=20,color = "black"),
        axis.title.y = element_text(size = 25,color = "black"),
        axis.text.y = element_text(size=20, color = "black"),
        legend.text = element_text(size = 15, color = "black"),
        legend.position = "bottom",  
        legend.direction = "horizontal",  
        legend.box = "horizontal",
        plot.margin = margin(0.5, 4, 0.5, 4, "cm")) 


p

  

