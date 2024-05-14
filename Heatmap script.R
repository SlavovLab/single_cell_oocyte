library(readxl)
library(ComplexHeatmap)

Ndata=as.matrix(read.csv("heatmap.csv",row.names = 1))
Age=read.csv("age_heatmap.csv",row.names = 1)


library(ComplexHeatmap)

fa = rep(c( "GV Young","GV AMA","MII Young","MII AMA"), times = c(12,14,17,11))
fa_col = c("GV Young"=12,"GV AMA"=14,"MII Young"=17,"MII AMA"=11)
dend1 = cluster_between_groups(Ndata, fa)


ha2<-HeatmapAnnotation (Age = anno_points(Age, size = unit(1.5, "mm"),height = unit(0.5,'cm'), pch = 16, abline(h = 18), gp = gpar(col = ifelse(Age > 30, "red", "black"))), na_col = "lightgrey", simple_anno_size = unit(5, "cm"), annotation_name_gp = gpar(fontsize = 10),annotation_legend_param =gpar(fontsize=10),annotation_name_side = "left")
p<-Heatmap(Ndata, cluster_columns = dend1,show_row_dend = FALSE,show_column_dend = FALSE,column_split = (4), row_labels = rownames(Ndata),
        row_title = "Differentially Abundant Proteins (DAPs)", cluster_rows=TRUE, row_names_gp = gpar(fontsize = 4),
        column_names_gp = gpar(fontsize = 4),show_row_names = FALSE, show_column_names=FALSE,top_annotation= ha2,heatmap_legend_param = list(title=expression("Log"[2]*"(Levels)")))
p

