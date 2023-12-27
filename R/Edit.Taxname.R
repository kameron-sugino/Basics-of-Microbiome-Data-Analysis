Edit.Taxname<-function(taxname,level,sep=";"){
  taxname<-data.frame(taxname)
  taxname<-separate(taxname,col=1, into=c("Kingdom","Phylum","Class","Order","Family","Genus"), sep=";")
  if(grepl("Genus|1", level, ignore.case = T)){
    x<-ifelse(grepl("unclassified|uncultured",taxname$Genus), paste(taxname$Genus, taxname$Family), 
              paste(taxname$Genus))
  }
  if(grepl("Family|2", level, ignore.case = T)){
    x<-ifelse(grepl("unclassified|uncultured",taxname$Family), paste(taxname$Order, taxname$Family), 
              paste(taxname$Family))
  }
  if(grepl("Order|3", level, ignore.case = T)){
    x<-ifelse(grepl("unclassified|uncultured",taxname$Order), paste(taxname$Class, taxname$Order), 
              paste(taxname$Order))
  }
  if(grepl("Class|4", level, ignore.case = T)){
    x<-ifelse(grepl("unclassified|uncultured",taxname$Class), paste(taxname$Phylum, taxname$Class),
              paste(taxname$Class))
  }
  if(grepl("Phylum|5", level, ignore.case = T)){
    x<-paste(taxname$Phylum)
    
  }
  x<-gsub("\\([[:digit:]]+\\)","",x)
  return(x)
}

