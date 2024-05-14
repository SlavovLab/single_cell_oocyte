
#Adjust names of files/ lab titles/x & y limits/ geom text for each subunit



library(ggplot2)
library(ggpubr)
data=read.csv('PRS10.csv')


p<-ggplot(data, aes(x=Age, y=PRS10)) +
  geom_point()+
  geom_smooth(method=lm,se=TRUE,colour= 'black')+
   scale_x_continuous(limits = c(18,44),n.breaks =8 )+
  scale_y_continuous(limits = c(-0.6,0.6),n.breaks=9)+
  theme_light()+
  theme(plot.title = element_text(hjust = 0.5), 
        legend.position="right", 
        legend.title = element_blank(),
        legend.text = element_text(), axis.text.x = element_text(size = 10)) +
  labs(x="Age (years)",
        y = expression("PRS10 (Log"[2]*"Levels)"),
       title="")  +
  theme(panel.grid = element_blank(),
        panel.border = element_rect(linewidth = 2,color = "black"),
       axis.title.x = element_text(size = 25,color = "black"), 
       axis.text.x = element_text(size=20, color = 'black'),
       axis.title.y = element_text(size = 25,color = "black"),
       axis.text.y = element_text(size=20,color = "black"),
       plot.margin = margin(0.5, 4, 0.5, 4, "cm"))+
  geom_text(label='R=-0.5, p<0.01',y = 0.6,x=31,size=8,colour='black',alpha = 0.1,fontface='italic')
  

p



