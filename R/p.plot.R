p.plot<-function(NB.pair,title="Plotted p-values"){
  taxa<-NB.pair[,1]
  p<-NB.pair[,-1]
  par(mar=c(10,4,1,1))
  plot(p[,1],xaxt = "n",ylim=c(0,1),xlab="",pch=16,ylab="p-value",main=paste(title))
  text(x=c(1:length(taxa)), y=par()$usr[3]-0.1*(par()$usr[4]-par()$usr[3]),
       labels=taxa, srt=45, adj=1, xpd=TRUE)
  legend(1,.95,legend=paste(colnames(p)), pch=16,col=seq(1,ncol(p)))
  axis(1, at=1:nrow(p), labels=FALSE)
  abline(h=0.05)
  for(i in 2:ncol(p)-1){
    par(new=TRUE)
    plot(jitter(1:nrow(p)),p[,i+1],ylim=c(0,1),xaxt = "n",pch=16,xlab="",yaxt = "n",ylab="",col=c(i+1))
  }
}