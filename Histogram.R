
#Adjust name of files/bins/x & y limits/ geom text
library(ggplot2)
data=read.csv('Proteasome_GV_Histogram.csv')


p1<-ggplot(data, aes(x=Randomised.Permuations)) +
  geom_histogram(aes(y=(..count..)/sum(..count..)),alpha=0.5, bins =50,show.legend = TRUE)+
  scale_x_continuous(limits = c(-0.2,0.2))+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1L))+
  geom_vline(xintercept=-0.14,linetype = "dashed",color='black',show.legend = TRUE)+
  theme_light()+
  labs(x="Correlations(R)",
       y='Frequency(%)',
       title="")  +
  theme(panel.grid = element_blank(),
        panel.border = element_rect(linewidth = 2,color = "black"),
        axis.title.x = element_text(size = 25,color = "black"), 
        axis.text.x = element_text(size=20, color = 'black'),
        axis.title.y = element_text(size = 25,color = "black"),
        axis.text.y = element_text(size=20,color = "black"),
        plot.margin = margin(0.5, 4, 0.5, 4, "cm"))+
  geom_text(aes(x=-0.15, y=0.05, label="Data"), color="black", angle=90, vjust=-0.5, hjust=0,size=5) +
  geom_text(aes(x=0.1, y=0.1, label="Randomised Permutations"), color="grey",size=5) 

  

p1








  
  ggtitle("Proteasome_complex rs distribution of Randomised Data ")+ 
  theme(plot.title = element_text(hjust = 0.5))+
  xlab("Randomised R")+
  ylab('Frequency')+
  theme_light()+
  theme(plot.title = element_blank(), 
        legend.position="right", 
        legend.title = element_blank(),
        legend.text = element_text(size=5))+
  theme(aspect.ratio = 1)+
  geom_text(aes(x=-0.42,label="R (original data) ", y=0.15), colour="black", angle=90, text=element_text(size=1))
  

 p1 
 
 
  rect <- data.frame(xmin=-0.15, xmax=-0.1, ymin=-Inf, ymax=Inf)
 PA<-p1+ geom_rect(data=rect, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax),
               color="transparent",fill='#fffbcc',
               alpha=0.3,
               inherit.aes = FALSE)
 
 
 
   
 
 
 PA

 p2<-ggplot(data, aes(x=Rs.Values., fill= Correlation.Coefficient)) +
   scale_fill_manual(values= c('red',"grey",'blue'))+
   geom_histogram(position="identity",alpha=0.5, bins = 100)+
   scale_x_continuous(limits = c(-0.15,-0.1))+
   scale_y_continuous(limits = c(0,4),breaks = seq(0,4,1))+
   #geom_vline(xintercept=c(-0.14,0.262107148),linetype = "dashed")+
   ggtitle("Proteasome_complex rs distribution of Randomised Data ")+ 
   theme(plot.title = element_text(hjust = 0.5))+
   xlab("rs")+
   theme_light()+
   theme(plot.title = element_blank(), 
         legend.position="right", 
         legend.title = element_blank(),
         legend.text = element_text())+
   theme(aspect.ratio = 0.95)
p2

PB<-p2+ geom_rect(data=rect, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax),
              color="transparent",fill='#fffbcc',
              alpha=0.3,
              inherit.aes = FALSE)

PB


ggarrange(PA, PB,common.legend = TRUE, legend="bottom")

 
 
 