NB.pairwise<-function(newOTUS,Group){
  Group<-as.factor(Group)
  grp<-length(levels(Group))
  otu.name<-colnames(newOTUS)
  p.vals<-data.frame()
  comp<-c()
  for(i in 1:grp){
    if(levels(Group)[1]!=levels(Group)[i]){
      comp<-c(comp,paste(levels(Group)[1],"vs",levels(Group)[i]))
    }
  }
  for (i in 1:ncol(newOTUS)){
    l<-glm.nb(newOTUS[,i]~Group)
    m<-data.frame(coef(summary(l))[,4][2:length(levels(Group))])
    j<-1
    while(j!=grp){
      p.vals[i,j]<-m[j,]
      j<-j+1
    }
  }
  for(i in 1:(grp-1)){
    p.vals[,i]<-p.adjust(p.vals[,i], method="BH")
  }
  overall<-cbind(otu.name,p.vals)
  colnames(overall)<-c("Taxa",comp)
  return(overall)
}