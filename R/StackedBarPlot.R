StackedBarPlot<-function(OTU,Group="Samples",TaxName,N=19,Title="Stacked Bar Chart"){
  Rowsum<-as.matrix(rowSums(OTU))
  abun<-matrix(0,nrow=nrow(OTU),ncol=ncol(OTU))
  for (i in 1:nrow(OTU)){
    for (j in 1:ncol(OTU)){
      abun[i,j]=(OTU[i,j])/(Rowsum[i])*100
    }
  }
  colnames(abun)<-TaxName
  abun<-abun[,order(-colSums(abun))]
  taxa_list<-colnames(abun)[1:N]
  taxa_list<-taxa_list[!grepl("unclassified unclassified",taxa_list)]
  N<-length(taxa_list)
  new_x<-data.frame(abun[,colnames(abun) %in% taxa_list],Others=rowSums(abun[,!colnames(abun) %in% taxa_list]))
  if (ncol(new_x)>(N+1)){
    Other<-rowSums(new_x[,c((N+1):ncol(new_x))])
    new_x<-new_x[,c(1:N)]
    new_x$Other<-Other
  }
  abun_groups<-cbind(Group,new_x)
  new_x <- abun_groups
  grouping_info<-new_x$Group
  new_x2<-new_x[,-1]
  tempname<-c(taxa_list,"Other")
  colnames(new_x2)<-tempname
  df<-NULL
  for (i in 1:dim(new_x2)[2]){
    tmp<-data.frame(row.names=NULL,Sample=rownames(new_x2),Taxa=rep(colnames(new_x2)[i],dim(new_x2)[1]),Value=new_x2[,i],Type=grouping_info)
    if(i==1){df<-tmp} else {df<-rbind(df,tmp)}
  }
  colours <- c("#F0A3FF", "#0075DC", "#993F00","#4C005C","#2BCE48","#FFCC99","#808080","#94FFB5","#8F7C00","#9DCC00","#C20088","#003380","#FFA405","#FFA8BB","#426600","#FF0010","#5EF1F2","#00998F","#740AFF","#990000","#FFFF00");
  p<-ggplot(df,aes(Sample,Value,fill=Taxa))+geom_bar(stat="identity")+facet_grid(. ~ Type, drop=TRUE,scale="free",space="free_x")
  p<-p+scale_fill_manual(values=colours[1:(N+1)])
  p<-p+theme_bw(base_size = 24)+ylab("Relative Abundance")+ggtitle(Title)+xlab("Sample ID")
  p<-p+ scale_y_continuous(expand = c(0,0))+theme(strip.background = element_rect(fill="gray85"))+theme(panel.spacing = unit(0, "lines"))
  p<-p+theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))+theme(axis.title.x=element_blank(),axis.text.x=element_blank(),axis.ticks.x=element_blank())
  print(p)
  return(df)
}