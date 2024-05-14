
#Adjust name of file and Define Group1 and Group2 

library(reshape2)
library(readxl)

obj = read.csv("GVYOUNG_MIIYOUNG.csv", row.names = 1)
colnames(obj)[1:13]="Group1"
colnames(obj)[14:29]="Group2"

Labels=as.factor(colnames(obj) )

obj=t(obj)
df=melt(obj)




library(enrichR)
websiteLive <- TRUE
dbs <-
  c(
    "KEGG_2021_Human",
    "WikiPathway_2021_Human",
    "BioPlanet_2019",
    "BioCarta_2016",
    "Reactome_2016",
    "MSigDB_Hallmark_2020",
    "GO_Biological_Process_2021",
    "GO_Molecular_Function_2021",
    "GO_Cellular_Component_2021",
    "MGI_Mammalian_Phenotype_Level_4_2021",
    "Human_Phenotype_Ontology",
    "Jensen_DISEASES",
    "DisGeNET",
    "DSigDB",
    "DrugMatrix",
    "OMIM_Disease",
    "HDSigDB_Human_2021",
    "COVID-19_Related_Gene_Sets_2021"
  )

enriched <- enrichr(colnames(obj)[],"GO_Biological_Process_2021")



Paths=list()
SplitsNames=list()
Overlap=vector()
Genes=vector()
for ( i in 1:nrow(enriched[["GO_Biological_Process_2021"]])){
  Terms=enriched[["GO_Biological_Process_2021"]][[1]][i]
  Overlap=append(Overlap,enriched[["GO_Biological_Process_2021"]][[2]][i])
  Genes=append(Genes,enriched[["GO_Biological_Process_2021"]][[9]][i])
  paths=unlist(strsplit(enriched[["GO_Biological_Process_2021"]][i,9],";"))
  Paths=append(Paths,list(paths))
  Names=unlist(Paths[[i]])
  SplitsNames=append(SplitsNames,list(Names)) 
  names(SplitsNames)[i]=Terms
  
}



Pvalue=vector()
FC=vector()
for ( i in seq_along(SplitsNames)){
  Groups=df[which(df$Var2  %in% SplitsNames[[i]]),]
  
  Wilcoxon_Mann_Whitney_test <- wilcox.test(value ~ Var1,data=Groups)
  diff=aggregate(value ~ Var1, data = Groups, FUN = mean)[[2]][2]-aggregate(value ~ Var1, data = Groups, FUN = mean)[[2]][1]
  
  
  p_value=Wilcoxon_Mann_Whitney_test$p.value
  FC=append(FC,diff)
  
  Pvalue=append(Pvalue,p_value)
}
BH=p.adjust(Pvalue, method = "BH",n=length(Pvalue))



numbers <- strsplit(as.character(Overlap), "/")
numbers <- lapply(numbers, as.numeric)

Ratio <- sapply(numbers, function(x) x[1]/x[2])




TableDF=data.frame(Terms=names(SplitsNames),Overlap=Overlap,Ratio=Ratio,Pvalue=Pvalue,BH=BH,FC=FC,Genes=Genes)

write.csv(TableDF,"NAME.csv")
