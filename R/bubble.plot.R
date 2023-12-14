bubble.plot<-function(OTUs_group1,OTUs_group2,ids_group1,ids_group2){
  rel_1<-OTUs_group1/rowSums((OTUs_group1))*100
  colnames(rel_1)<-TaxName
  rel_2<-OTUs_group2/rowSums((OTUs_group2))*100
  colnames(rel_2)<-TaxName
  
  rel_1<-rel_1[ids_group1%in%ids_group2,]
  rel_2<-rel_2[ids_group2%in%ids_group1,]
  
  ids<-ids_group1[ids_group1%in%ids_group2]
  ids<-ids[ids%in%ids_group1]
  
  rel.12<-rel_1[,-c(1:3)]-rel_2[,-c(1:3)]
  rel.12.col<-colSums(rel.12)
  
  temp<-NULL
  for (i in 1:ncol(rel.12)){
    if(abs(min(rel.12[,i]))<=1){
      temp<-c(temp,i)
    }
  }
  rel.12.edit<-rel.12[,-c(temp)]
  rel.12.edit<-rel.12.edit[,ncol(rel.12.edit):1]
  rel.12.edit<-data.frame(ids,rel.12.edit)
  
  
  df <- reshape2::melt(rel.12.edit)
  circles<-ifelse(df$value>0,20,ifelse(df$value<0,21,NA))
  size<-ifelse(abs(df$value)>=1&abs(df$value)<5,1,ifelse(abs(df$value)>=5&abs(df$value)<10,2,ifelse(abs(df$value)>=10&abs(df$value)<20,3,ifelse(abs(df$value)>=20,4,0))))
  
  par(mar=c(1,15,5,3), xpd=T)
  plot(y=as.integer(df$variable), x=as.integer(factor(df$ids)), pch=circles, cex=size, bty="n", axes = F, xlab="", ylab="")
  axis(2, at = unique(as.integer(df$variable)), labels = levels(df$variable), line = 0.5, las=2)
  axis(3, at = unique(as.integer(factor(df$ids))), labels = levels(factor(df$ids)), line = 0.5,las=2)
  legend(y=max(as.integer(df$variable)/1.5), x=max(as.integer(df$ids))+0.25, legend = c("+20%","+10%","+5%","+1%","~0%","-1%","-5%","-10%","-20%"), pch = c(20,20,20,20,0,21,21,21,21), bty="n", pt.cex=c(4,3,2,1,0,1,2,3,4))
}