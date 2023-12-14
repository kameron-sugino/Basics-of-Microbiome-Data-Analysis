#' Subsample
#'
#' rarefies an otu table of counts to a specified minimum number of counts
#'
#' @param OTU table containing count data to be rarefied
#' @param Min total number of counts pulled per sample (i.e., row); defaults to the sample with the lowest number of counts
#' @param Iters number of times the data is subsampled before averaging the data and rounding to the nearest integer; defaults to 999 iterations 
#'
#' @return rarefied table of counts
#'
#' @examples
#' Subsample(OTU=otu.table, Min=min(rowSums(OTU)),Iters=999)
#' 
Subsample<-function(OTU,Min=min(rowSums(OTU)),Iters=999){
  results<-matrix(0,nrow=nrow(OTU),ncol=ncol(OTU))
  pb <- txtProgressBar(min=0, max=Iters, style=3)
  for (i in 1:Iters){
    temp<-rrarefy(x=OTU, sample=Min)
    results<- temp + results
    setTxtProgressBar(pb, i)
  }
  results<-round(results/Iters)
  close(pb)
  return(results)
}
