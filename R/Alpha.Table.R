Alpha.Table<-function(alpha,group,...){
  a<-alpha$Chao
  b<-alpha$Shannon
  c<-alpha$Invsimpson
  d<-group
  c.<-c(paste(round(mean(a),1),"\u00b1",round(sd(a),1)),paste(round(aggregate(a,list(d),mean)[,2],1),"\u00b1",round(aggregate(a,list(d),sd)[,2],1)))
  s.<-c(paste(round(mean(b),1),"\u00b1",round(sd(b),1)),paste(round(aggregate(b,list(d),mean)[,2],1),"\u00b1",round(aggregate(b,list(d),sd)[,2],1)))
  i.<-c(paste(round(mean(c),1),"\u00b1",round(sd(c),1)),paste(round(aggregate(c,list(d),mean)[,2],1),"\u00b1",round(aggregate(c,list(d),sd)[,2],1)))
  
  alpha.table<-rbind(c.,s.,i.)
  colnames(alpha.table)<-c("Overall",levels(factor(group)))
  return(alpha.table)
}