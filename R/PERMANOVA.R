PERMANOVA<-function(OTUS,Group,binary,iters=9999){
  Data.Dist<-vegdist(OTUS,method="bray", binary=binary)
  adonis(Data.Dist~Group,permutations=iters)
}