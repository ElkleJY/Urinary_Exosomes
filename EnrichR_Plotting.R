enrich=read.csv(file = 'Pictures/miR221_pathway.csv')
type="group1"
rna="miR221-222"
group="1"

enrich_signif=enrich[which(enrich$P.value<0.05),]
enrich_signif=enrich[which(enrich$adj.p<0.25),]
enrich_signif=enrich_signif[enrich_signif$group==group,]
enrich_signif=data.frame(enrich_signif)

library(ggplot2)
library(dplyr)

######
data=enrich_signif[1:min(30,nrow(enrich_signif)),]
data<-data%>%arrange(Odds.Ratio)
rownames(data)<-1:length(data$Term)
data$Term<-gsub("Homo sapiens P.....","",data$Term)

Term_order=factor(as.integer(rownames(data)),labels = data$Term)

p<-ggplot(data=data,aes(x=Odds.Ratio,y=Term_order,fill=adj.p))+
  geom_bar(stat = "identity",width = 0.8)+
  theme_bw()+
  ylab("Terms")+
  xlab("Odds.Ratio")+
  labs()+
  ggtitle(rna)

show(p)
pdf(paste0("Pictures/",rna,"_EnrichR_",type,".pdf"),height = 3.2,width = 8.5)
print(p)
dev.off()
