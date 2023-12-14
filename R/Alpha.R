Alpha<-function(OTU,Names="Sample",Groups="Sample"){
  Chao<-t(estimateR(OTU))
  Chao<-Chao[,2]
  Shannon<-diversity(OTU,index="shannon")
  Invsimpson<-diversity(OTU,index="invsimpson")
  OTU.Subsample.Alpha<-data.frame(Names,Groups,Chao,Shannon,Invsimpson)
  return(OTU.Subsample.Alpha)
}