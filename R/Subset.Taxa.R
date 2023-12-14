Subset.Taxa<-function(OTUS,TaxName,CutOff=1){
  colnames(OTUS)<-TaxName
  row<-rowSums(OTUS)
  row<-sum(row)
  col<-colSums(OTUS)
  ratio<-as.matrix(col/row*100)
  ratio<-cbind(TaxName,ratio)
  subset<-data.frame(ratio[ratio[,2]>=CutOff,])
  subset<-data.frame(subset[!subset$X1=="unclassified unclassified",])
  subset<-data.frame(subset[!grepl("uncultured_ge",subset$X1),])
  newOTUS<-data.frame(OTUS[,colnames(OTUS) %in% subset$X1])
  colname<-colnames(newOTUS)
  colnames(newOTUS)<-gsub("\\."," ",colname)
  return(newOTUS)
}