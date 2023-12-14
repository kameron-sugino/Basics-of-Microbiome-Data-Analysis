Edit.Taxname<-function(n,level){
  if(level=="Genus"|level==1){
    n<-as.matrix(n)
    for (i in 1:4){
      n<-gsub('^.*?;', '', n)
    }
    n<-gsub(';',' ',n)
    n<-gsub('\\(100)','',n)
    n<-data.frame(n)
    n<-separate(n, col=1,into=c("Family","Genus"), sep=" ")
    x<-ifelse(n$Genus%in%c("unclassified","uncultured"), paste(n$Genus, n$Family), paste(n$Genus,n$Other1,n$Other2))
    n<-as.matrix(x)
    return(n)
  }else if(level=="Family"|level==2){
    n<-as.matrix(n)
    for (i in 1:3){
      n<-gsub('^.*?;', '', n)
    }
    n<-gsub(';',' ',n)
    n<-gsub('\\(100)','',n)
    n<-data.frame(n)
    n<-separate(n,col=1, into=c("Order","Family","Genus"), sep=" ")
    x<-ifelse(n$Family%in%c("unclassified","uncultured"), paste(n$Order, n$Family), paste(n$Family))
    n<-as.matrix(x)
    return(n)
  }else if(level=="Order"|level==3){
    n<-as.matrix(n)
    for (i in 1:2){
      n<-gsub('^.*?;', '', n)
    }
    n<-gsub(';',' ',n)
    n<-gsub('\\(100)','',n)
    n<-data.frame(n)
    n<-separate(n,col=1, into=c("Class","Order","Family","Genus"), sep=" ")
    x<-ifelse(n$Order%in%c("unclassified","uncultured"), paste(n$Class, n$Order), paste(n$Order))
    n<-as.matrix(x)
    return(n)
  }else if(level=="Class"|level==4){
    n<-as.matrix(n)
    for (i in 1){
      n<-gsub('^.*?;', '', n)
    }
    n<-gsub(';',' ',n)
    n<-gsub('\\(100)','',n)
    n<-data.frame(n)
    n<-separate(n,col=1, into=c("Phylum","Class","Order","Family","Genus"), sep=" ")
    x<-ifelse(n$Class%in%c("unclassified","uncultured"), paste(n$Phylum, n$Class), paste(n$Class))
    n<-as.matrix(x)
    return(n)
  }else if(level=="Phylum"|level==5){
    n<-as.matrix(n)
    n<-gsub('[(0-9);""]{1,}', '_', n)
    n<-gsub('^.*?_', '', n)
    n<-gsub('_.*', '', n)
  }
}