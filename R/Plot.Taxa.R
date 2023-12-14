Plot.Taxa<-function(OTUS,Data.PCOA,TaxName,CutOff=1,pval=0.05,...){
  colnames(OTUS)<-TaxName
  row<-rowSums(OTUS)
  row<-sum(row)
  col<-colSums(OTUS)
  ratio<-as.matrix(col/row*100)
  ratio<-cbind(TaxName,ratio)
  subset<-data.frame(ratio[ratio[,2]>=CutOff,])
  subset<-data.frame(subset[!subset$X1=="unclassified unclassified",])
  newOTUS<-data.frame(OTUS[,colnames(OTUS) %in% subset$X1])
  colname<-colnames(newOTUS)
  colnames(newOTUS)<-gsub("\\."," ",colname)
  fit<-envfit(Data.PCOA, newOTUS)
  plot(fit, p.max=pval,...)
  fit$vectors$r[fit$vectors$pvals<0.05]
}