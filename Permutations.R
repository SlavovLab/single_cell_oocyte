
#Adjust files name/Number of permuations

data <- as.data.frame(read.csv("GV_PERMUTATIONS.csv",row.names = 1) )
data=as.data.frame(t(data))
age <- read.csv("AGE_GV_PERMUTATIONS.csv")

# Define the bootstrapping function

bootstrap <- function(data, num_resamples) {
  resamples <- vector("list", num_resamples)
  AgeLista<- vector("list", num_resamples)
  for (i in 1:num_resamples) {
    print(i)
    # sample rows and columns from the dataframe with replacement
    resamples[[i]] <- data[sample(nrow(data), replace = FALSE), sample(ncol(data), replace = FALSE)]
    AgeLista[[i]] <- age[sample(nrow(age), replace = FALSE),]
    
    }
  return(list(resamples,AgeLista) )
}


# Call the bootstrapping function
bootstraps <- bootstrap(data, 100000)

for ( i in 1:length(bootstraps[[1]])){
  print(i)
  rownames(bootstraps[[1]][[i]])=rownames(data)
  colnames(bootstraps[[1]][[i]])=colnames(data)
}

for ( i in 1:length(bootstraps[[1]])){
  print(i)
  bootstraps[[1]][[i]]$age=bootstraps[[2]][[i]]

}


#  saveRDS(bootstraps,"bootstraps.rds")


##############Correlation Spearman##################
#Adjust the name of the protein you want to calculate

corList=vector("list", length(bootstraps[[1]]) )
for ( i in 1:length(bootstraps[[1]] ) ){

  corList[[i]]=cor.test(bootstraps[[1]][[i]]$age, bootstraps[[1]][[i]]$P62195,method="spearman")
  
}



PvalueList=c()
rhoList=c()
for ( i in seq_along(corList) ){
  
  PvalueList=append(PvalueList,corList[[i]][["p.value"]] ) 
  rhoList=append(rhoList,corList[[i]][["estimate"]][["rho"]])
}
# BH Correction
BHcor <- (p.adjust(PvalueList, method = "BH",n=length(PvalueList)))
BHcor=as.data.frame(BHcor)

SpearmanResults=as.data.frame(cbind(PvalueList,BHcor,rhoList) )
write.csv(SpearmanResults,"NAMEOFFILE.csv",)
