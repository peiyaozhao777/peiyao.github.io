#library(psych)
#library(xlsx)
library(cluster)
#library(RColorBrewer)
library(data.table)
library(dendextend)
library(ggplot2)
#library(NbClust)
library(scales)
library(extrafont)
library(gridExtra)
library(stats)
#setwd('Projects/covid-analysis/')

d <- read.csv('../results/disimilarity-matrix-mobility.csv',row.names = 1)
num_clust = 4
clusterFit <- function(dm, m) {
  fit <- hclust(as.dist(dm), method=m) # apply hierarchical clustering 
  #fit$labels = f8scores$city
  return(fit)
}

## Get clustering for 8 and 9-factor solutions
results = clusterFit(transpose(d), "ward.D")

#optClustWardScaled9 <- NbClust(as.data.frame(scaled.f9scores),distance="manhattan", method="ward.D2",min.nc=10,max.nc=15,index="gap")
#optClustWardScaled9 <- NbClust(diss=dis9scaled.mat, method="ward.D2",min.nc=7,index="all")


## Group countries into clusters based on number of clusters desired
getClusters <- function (clusfit, k, labels) {
  #cluster <- dendextend:::cutree(clusfit, k, order_clusters_as_data = TRUE)
  cluster = cutree(clusfit, k)
  clusters = data.frame(cluster)
  row.names(clusters) = labels
  return(clusters)
}

clustered.data <- getClusters(results, num_clust, row.names(d)) #f9scores only for names!!
clustered.data.ordered <- clustered.data[order(clustered.data$cluster),,drop=FALSE]
write.csv(clustered.data.ordered, "../results/country-clusters.csv")

colors = c( # '#ffff99', ##d8ac93', # '#ffff99', #or yellowversions 
    '#66c2a5',
    '#fc8d62',
    '#8da0cb',
    '#e78ac3',
    '#a6d854',
    '#ffd92f',
    '#e5c494')

colors = colors[1:num_clust]

dendtypolist = c('C1', 'C2', 'C3', 'C4', 'C5', 'C6', 'C7')


##################################################################################################################
#### Plot dendrogram
##################################################################################################################
# https://gist.github.com/jslefche/eff85ef06b4705e6efbc
theme_black = function(base_size = 12, base_family = "") {
  
  theme_grey(base_size = base_size, base_family = base_family) %+replace%
    
    theme(
      # Specify axis options
      axis.line = element_blank(),  
      axis.text.x = element_text(size = base_size*0.8, color = "white", lineheight = 0.9),  
      axis.text.y = element_text(size = base_size*0.8, color = "white", lineheight = 0.9),  
      axis.ticks = element_line(color = "white", size  =  0.2),  
      axis.title.x = element_text(size = base_size, color = "white", margin = margin(0, 10, 0, 0)),  
      axis.title.y = element_text(size = base_size, color = "white", angle = 90, margin = margin(0, 10, 0, 0)),  
      axis.ticks.length = unit(0.3, "lines"),   
      # Specify legend options
      legend.background = element_rect(color = NA, fill = "black"),  
      legend.key = element_rect(color = "white",  fill = "black"),  
      legend.key.size = unit(1.2, "lines"),  
      legend.key.height = NULL,  
      legend.key.width = NULL,      
      legend.text = element_text(size = base_size*0.8, color = "white"),  
      legend.title = element_text(size = base_size*0.8, face = "bold", hjust = 0, color = "white"),  
      legend.position = "right",  
      legend.text.align = NULL,  
      legend.title.align = NULL,  
      legend.direction = "vertical",  
      legend.box = NULL, 
      # Specify panel options
      panel.background = element_rect(fill = "black", color  =  NA),  
      panel.border = element_rect(fill = NA, color = "white"),  
      panel.grid.major = element_line(color = "grey35"),  
      panel.grid.minor = element_line(color = "grey20"),  
      panel.margin = unit(0.5, "lines"),   
      # Specify facetting options
      strip.background = element_rect(fill = "grey30", color = "grey10"),  
      strip.text.x = element_text(size = base_size*0.8, color = "white"),  
      strip.text.y = element_text(size = base_size*0.8, color = "white",angle = -90),  
      # Specify plot options
      plot.background = element_rect(color = "black", fill = "black"),  
      plot.title = element_text(size = base_size*1.2, color = "white"),  
      plot.margin = unit(rep(1, 4), "lines")
      
    )
  
}

plotD <- function (clusfit,kk,meth,labels) 
{
  nodeP <- list(lab.cex = 0.8, pch = c(NA, NA), cex = 1.5,font=1)
  dend <- as.dendrogram(clusfit)
  #dend <- reorder(dend, 1:13, mean)
  labels(dend) <- as.character(labels[clusfit$order])
  dend <- set(dend, "labels_cex", .65)
  #dend <- set(dend, "hang_leaves", 1)
  d1=color_branches(dend,k=kk,col = colors) # brewer.pal(kk,"Paired"))
  dcol <- get_leaves_branches_col(d1)
  d1 <- color_labels(d1,k=kk,col=colors) #brewer.pal(kk,"Paired"))
  png(file=paste0("../results/Dendrogram-",kk,"-clusters-","-Method-",meth,".png"),family="CM Sans", width=2400,height=2000, res=240) #5300 #2600
  plot(d1)
  #colored_bars(dcol, dend, rowLabels = c("13 Typologies"))
  dev.off()
}

plotD(results,num_clust,"Ward.D2",row.names(d))

# plotDgg <- function (clusfit,kk,meth,scores,ff) 
# {
#   dend <- as.dendrogram(clusfit)
#   dend <- assign_values_to_branches_edgePar(dend=dend, value = "white", edgePar = "col")
#   dend <- color_branches(dend,k=kk,col = colors) # brewer.pal(kk,"Paired"))
#   ggd1 <- as.ggdend(dend)
#   base <- ggplot(ggd1,labels=FALSE)
#   base + 
#     geom_hline(yintercept=6.2, color="gray",lty=2,lwd=1) + 
#     geom_hline(yintercept=3.6, color="gray",lty=3,lwd=1) + 
#     theme_black() +
#     theme(panel.border = element_blank(),
#             panel.grid.major = element_blank(),
#             panel.grid.minor = element_blank(),
#             axis.title=element_blank(),
#             axis.ticks=element_blank(),
#             axis.text=element_blank(),
#             legend.position=c(0,1)
#           )
# }
# p1 <- plotDgg(results,num_clust,"Ward.D2",row.names(d))

# idx = c(1:length(row.names(d)))
# df2<-data.frame(cluster=cutree(results,num_clust) ,states=factor(idx)) #,levels=idx)) #[clusfit9scaled.ward$order]))
# head(df2)
# df3 <- df2[order(df2$cluster),]
# df3$states <- factor(1:length(row.names(d)))
# df3$cluster <- factor(df3$cluster, labels = dendtypolist[1:num_clust])

# p2 <- ggplot(df3,aes(states,y=1,fill=factor(cluster)))+
#   geom_tile()+
#   #scale_y_continuous(expand=c(0,0)) + 
#   scale_fill_manual(values = colors,breaks=dendtypolist[1:num_clust])+  
#   #scale_fill_discrete( #+ scale_fill_manual(values = colors)+
#   theme(axis.title=element_blank(),
#         axis.ticks=element_blank(),
#         axis.text=element_blank(),
#         legend.position=c(.85,9.3),
#         legend.title=element_blank(),
#         legend.text = element_text(colour="white", size=16),
#         legend.key.size = unit(1.5, 'lines'),
#         legend.key = element_rect(fill = "gray", colour = "transparent"),
#         legend.background = element_rect(fill = "transparent", colour = "transparent")
#         )

# ggplotGrob(p1)
# #gp2<-ggplotGrob(p2)  

# #maxWidth = grid::unit.pmax(gp1$widths[2:5], gp2$widths[2:5])
# #gp1$widths[2:5] <- as.list(maxWidth)
# #gp2$widths[2:5] <- as.list(maxWidth)

# loadfonts()
# png(file=paste0("results/Dendrogram-GG-",num_clust,"-clusters-","-Method-","Ward.D2",".png"),family="CM Sans",width=1200,height=1000,res=100)
# #grid.arrange(gp1, gp2, ncol=1,heights=c(9/10,1/10))

# dev.off()

