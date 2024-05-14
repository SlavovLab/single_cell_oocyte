#Adjust files name

data=read.csv("MIIYOUNG_MIIAMA_SPEARMAN.csv")

rownames(data)=data[,1]
data=data[,-1]
data=as.data.frame(t(data)) 


corList=list()
for ( i in 1:(ncol(data)-1) ){
  
  corList[[i]]=cor.test(data$Age, data[,i],method="spearman")
}
names(corList)=colnames(data[,-ncol(data)])

PvalueList=c()
rhoList=c()
for ( i in seq_along(corList) ){
  
  PvalueList=append(PvalueList,corList[[i]][["p.value"]] ) 
  rhoList=append(rhoList,corList[[i]][["estimate"]][["rho"]])
}
# BH Correction
Bcor <- (p.adjust(PvalueList, method = "BH",n=length(PvalueList)))
Bcor=as.data.frame(Bcor)

SpearmanResults=as.data.frame(cbind(PvalueList,Bcor,rhoList) )
rownames(SpearmanResults)=names(corList)

SpearmanResults=SpearmanResults[order(SpearmanResults$Bcor,decreasing = FALSE),]

write.csv(SpearmanResults,"nameoffile.csv",)
