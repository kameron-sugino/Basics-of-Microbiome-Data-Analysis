paired.arrows<-function(pcoa.df,id,group,term1,term2,lwd=1){
  test<-cbind(paste0(as.character(group),"_-_",id),pcoa.df)
  a<-data.frame(test)
  a[,1]<-as.character(a[,1])
  a[,2]<-as.numeric(as.character(a[,2]))
  a[,3]<-as.numeric(as.character(a[,3]))
  test<-a
  for(i in (1:nrow(test))){
    for(j in (1:nrow(test))){
      if(gsub('.*_-_',"",test[i,1])==gsub('.*_-_',"",test[j,1])&gsub('_-_.*',"",test[i,1])==paste(term1)&gsub('_-_.*',"",test[j,1])==paste(term2)){
        arrows(x0=test[i,2],y0=test[i,3],x1=test[j,2],y1=test[j,3],lty=1,length=0.1,lwd=lwd)
      }
    }
  }
}