PERMDISP<-function(OTUS,Group,binary,iters=9999){
  Data.Dist<-vegdist(OTUS,method="bray", binary=binary)
  Data.betadisper<-betadisper(Data.Dist, group=Group)
  permutest(Data.betadisper, group=Group, permutations=iters)
}