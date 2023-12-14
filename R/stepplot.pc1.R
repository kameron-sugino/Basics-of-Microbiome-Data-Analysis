stepplot.pc1<-function(sor.bray.df,sample_ids,col=1,pc="PC1",...){
  m<-factor(sample_ids)
  temp<-data.frame(cbind(sor.bray.df,m,col))
  temp[,1]<-as.numeric(as.character(temp[,c(1)]))
  temp[,2]<-as.numeric(as.character(temp[,c(2)]))
  
  temp2<-temp[with(temp, order(ave(temp[,1], temp[,3], FUN =min), temp[,1])), ]
  
  temp2$m<-factor(temp2$m, levels=unique(temp2$m))
  plot(temp2$V1,temp2$m,col=factor(temp2[,4]),xlab=pc,ylab="Participant ID",yaxt="n",...)
  axis(side = 2, at = 1:length(unique(temp2$m)),labels = unique(temp2$m),las=1)
}