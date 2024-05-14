
#Adjust files name/ define groups for wilcoxon

Data=as.data.frame(read.csv("GVYOUNG_MIIYOUNG_WILCOXON.csv"))
rownames(Data)=Data[,1]
Data=Data[,-1]



Wilcoxon_Mann_Whitney_test <- apply(Data, 1,function(i)wilcox.test(i[1:12],i[13:29], var.equal = TRUE))
p_value <- unlist(lapply(Wilcoxon_Mann_Whitney_test, function(x) x$p.value))

# BH Correction
Bcor <- (p.adjust(p_value, method = "BH",n=length(p_value)))
Bcor=as.data.frame(Bcor)

GroupW=vector()

for ( i in 1:length(Wilcoxon_Mann_Whitney_test)){
  GroupW[[i]]=Wilcoxon_Mann_Whitney_test[[i]][["statistic"]][["W"]]
  
}
names(GroupW)=names(Wilcoxon_Mann_Whitney_test)

GroupW=as.data.frame(GroupW)

colnames(GroupW)="W"

resultsWilco=cbind(p_value,Bcor,GroupW)


write.csv(resultsWilco,"nameoffile.csv")


