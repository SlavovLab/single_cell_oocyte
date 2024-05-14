
#Adjust name of files/x & y limits/levels/ scale_fill_manual


library(ggplot2)

data1=read.csv('TRIC_MII.csv')


data1$A<- factor(data1$A, levels = c('MII Young','MII AMA'))
p1<-ggplot(data1,aes(x=TRiC.Subunits,y=Log2.levels.,fill=A))+
  geom_boxplot()+
  scale_y_continuous(limits = c(-1,1))+
  theme_light()+
  theme(plot.title = element_text(hjust = 0.5), 
        legend.position="right", 
        legend.title = element_blank(),
        legend.text = element_text(), axis.text.x = element_text(size = 10)) +
  labs(x=" Subunits",
       y = expression("TRiC Complex (Log"[2]*"Levels)"),
       title="")  +
  theme(panel.grid = element_blank(),
        panel.border = element_rect(linewidth = 2,color = "black"),
        axis.title.x = element_text(size = 25,color = "black"), 
        axis.text.x = element_text(size=20, color = 'black'),
        axis.title.y = element_text(size = 25,color = "black"),
        axis.text.y = element_text(size=20,color = "black"),
        plot.margin = margin(0.5, 4, 0.5, 4, "cm"),
        legend.text = element_text(size = 15),
        legend.position = "bottom",  
        legend.direction = "horizontal",  
        legend.box = "horizontal")+
  scale_fill_manual(values = c('MII Young' = 'black', 'MII AMA' = 'red'))

p1
  
  
  
  
  
  
  