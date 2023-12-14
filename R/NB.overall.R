NB.overall<-function(newOTUS,Group){
  m<-as.matrix(NA)
  n<-as.matrix(NA)
  o<-as.matrix(NA)
  for (i in 1:ncol(newOTUS)){
    l<-glm.nb(newOTUS[,i]~Group)
    m<-anova(l)
    n[i]<-data.frame(m[2,5])
    o[i]<-colnames(newOTUS[i])
  }
  n<-p.adjust(n, method="BH")
  p<-cbind(o,n)
  return(p)
  p[,1]<-as.character(p[,1])
  p[,2]<-as.numeric(as.character(p[,2]))
  par(mar=c(10,4,1,1))
  plot(p[,2],xaxt = "n",ylim=c(0,1),xlab="",pch=16,ylab="p-value",main="Overall p-values")
  axis(1, at=1:nrow(p), labels=FALSE)
  text(x=c(1:nrow(p)), y=par()$usr[3]-0.1*(par()$usr[4]-par()$usr[3]),
       labels=p[,1], srt=45, adj=1, xpd=TRUE)
  abline(h=0.05)
}