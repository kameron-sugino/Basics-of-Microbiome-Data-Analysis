Sor.bray.pcoa<-function(OTUS,Dim=2,Color=1,binary,Title="PCoA",...){
  Data.df<-vegdist(OTUS,method="bray", binary)
  Data.df.PCoA<-cmdscale(Data.df, k = Dim, eig = FALSE)
  Data.df.PCoA.eig<-cmdscale(Data.df, k = Dim, eig = TRUE)
  eig.Data.df.PCoA<-Data.df.PCoA.eig$eig
  eig.Data.df.PCoA.sum<-sum(eig.Data.df.PCoA)
  a<-(eig.Data.df.PCoA/eig.Data.df.PCoA.sum)*100
  xlab<-paste("PC1","(",round(a[1],1),"%",")",sep="")
  ylab<-paste("PC2","(",round(a[2],1),"%",")",sep="")
  if(binary==TRUE){
    main<-"Sorensen PCoA"
  }else(main<-"Bray-Curtis PCoA")
  plot(Data.df.PCoA, col=Color,
       main=Title,xlab=xlab,ylab=ylab,...)
  return(Data.df.PCoA)
}