
library(ggplot2)
data= read.csv('dotplot.csv')
data$Group <- factor(data$Groups, levels = c("Young", "AMA"))
ggplot(data, aes(x = Group, y = Pathways, size = Ratio, color = P.adj)) +
  geom_point(alpha = 0.7) +  
  scale_size_continuous(trans = "log", range = c(2, 10)) +
  scale_color_gradient2(low = "#0b27f0", mid = "white", high = "#ff1500", midpoint = 0.015) +
  labs(x = "Group", y = "Pathways", size =    "Ratio 
(Identified proteins/ Total proteins", color = "P.adj") +
  theme(axis.title.x = element_text(size = 25),
  axis.title.y = element_text(size = 25),
  axis.text.x = element_text(size=20),
  axis.text.y = element_text(size=20),
  legend.text = element_text(size = 15))+
  theme_minimal()


