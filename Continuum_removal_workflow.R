f <-read_csv("test/products/fullSpectraChunks.csv")

full <- f%>%pivot_longer(f,cols = -depth.mid,names_to = "Wavelengths", values_to = "Intensity")
full$Wavelengths <- as.numeric(full$Wavelengths)

tf <- t(f)
colnames(tf)<- c("Depth_01","Depth_02","Depth_03")


#Continuum Removal
library(prospectr)
library(ggplot2)
cR <- prospectr::continuumRemoval(X=f[,-1],type=c("R","A"),interpol="spline")
colnames(cR)<-rownames(tf[-1,]) 
cR <- as.data.frame(cR)
cR$Depth <- c(1,2,3)
contRem <- cR%>%pivot_longer(cR,cols=-Depth,names_to = "Wavelengths", values_to = "Intensity")
contRem$Wavelengths <- as.numeric(contRem$Wavelengths)

ggplot(contRem,aes(x=Wavelengths,y=Intensity,color=Depth))+geom_point()+
  scale_x_continuous(breaks =round(seq(300,1100,by=50),1000))+theme_classic()




##Cluster analysis and PCA -not done yet!##
set.seed(123)

de <- ecodist::distance(t(f[,-1]),method="euclidean")
complete_fit <- hclust(de,method="complete")
names(f[,-1])
plot(complete_fit, labels = names(f[,-1]), las =2 )

#Second cluster method

#determine appropriate number of clusters for each method
nClusters_complete <- max(NbClust(data = t(f[,-1]), diss = NULL, distance = "euclidean",
                                  min.nc = 2, max.nc = 15, method = "complete")$Best.partition)

#calculate distances by cluster number for each method
final_complete <- hcut(t(f[,-1]), nClusters_complete, nstart = 25, hcfunc = "hclust",hc_method = "complete")
fviz_cluster(final_complete, data=t(f[,-1]))+theme_classic()


cluster_no_com <- as.factor(final_complete$cluster)

out_complete <- data.frame(cbind(t(f[,-1]),cluster_no_com))

ggplot(data=out_complete,aes(x=names(f[,-1]),y=V1,colour=cluster_no_com))+geom_point()


pca_res <- prcomp(f[,-1], scale. = TRUE)
plot2 <- autoplot(pca_res,label=TRUE,alpha=.1, loadings = TRUE,loadings.label=TRUE, loadings.label.size= 3,check_overlap=TRUE)+theme_classic()+ggtitle("PC Analysis")
autoplot(pca_res)

