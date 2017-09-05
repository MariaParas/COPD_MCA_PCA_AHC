# STEP 4: HC 
# comment: choose cont_mca=cbind(cont_p[,1:6],comorbid[,1:9],drug[,1:5]) or cont_mca=cbind(cont_p[,1:6],comorbid[,1:9]) for the main components
# obtained from DRUGS+sex, COMORBIDITIES+ CONTINUOUS or # sex, COMORBIDITIES+ CONTINUOUS
# Ward. D2 AHC usinG Euclidean distances
cont_mca=cbind(cont_p[,1:6],comorbid[,1:9],drug[,1:5])
#cont_mca=cbind(cont_p[,1:6],comorbid[,1:9])
head(cont_mca)
summary(cont_mca)

dim(cont_mca)
desc_stats <- data.frame(
  Min = apply(cont_mca, 2, min), # minimum
  Med = apply(cont_mca, 2, median), # median
  Mean = apply(cont_mca, 2, mean), # mean
  SD = apply(cont_mca, 2, sd), # Standard deviation
  Max = apply(cont_mca, 2, max) # Maximum
)
desc_stats <- round(desc_stats, 1)
desc_stats
# Dissimilarity matrix
df=scale(cont_mca)
summary(df)
d <- dist(cont_mca, method = "euclidean")

# Hierarchical clustering using Ward's.D2 method
res.hc <- hclust(d, method = "ward.D2" )
plot(res.hc) # display dendogram
groups <- cutree(res.hc, k=2) # cut tree into 2 clusters
# draw dendogram with red borders around the 2 clusters
rect.hclust(res.hc, k=2, border=3:8) 
plot(res.hc,xlab="") 
groups <- cutree(res.hc, k=2) # cut tree into 2 clusters
# draw dendogram with red borders around the 2vclusters
rect.hclust(res.hc, k=2, border=5:6 )

dev.off()

hcd = as.dendrogram(res.hc)

plot(hcd)
library(dendextend)
de <- hcd
de <- color_branches(de, k = 2)
de <- color_labels(de, k = 2)
plot(de,main="Dendogram with 2 clusters")


# cut the dendogram
grp <- cutree(res.hc, k =4)
# Number of members in each cluster
table(grp)
# Get the names for the members of cluster 1
rownames(df)[grp == 1]
library(factoextra)
# plot clusters for visualization
fviz_cluster(list(data = df, cluster = grp),pallete="Set2",geom="point",pointsize=1,
             ggtheme=theme_classic())

fviz_cluster(list(data = df, cluster = grp),palette = "Set2", ggtheme = theme_minimal())


# plot silhloutte statistic, within sum and GAP statistic
fviz_nbclust(cont_mca, FUN = hcut, method = "wss",hc_method = "ward.D2")
fviz_nbclust(cont_mca, FUN = hcut, method = "silhouette",hc_method = "ward.D2")
gap_stat <- clusGap(cont_mca, FUN = hcut, nstart = 25, K.max = 10, B = 50)
fviz_gap_stat(gap_stat)
res.hc2 <- eclust(cont_mca, "hclust", k = 6,
                  method = "ward.D2", graph = FALSE) 

# perform silhouette plots

fviz_silhouette(res.hc2)
fviz_dend(res.hc2, rect = TRUE, show_labels = FALSE) 
# combine the data set
dat=cbind(FINAL,cont_mca,grp)
head(dat)
dat1=subset(dat,grp==1)
dim(dat1)

summary(dat1)
dat2=subset(dat,grp==2)
dim(dat2)
summary(dat2)
dat3=subset(dat,grp==3)
dim(dat3)
summary(dat3)
dat4=subset(dat,grp==4)
dim(dat4)
summary(dat4)
#dat5=subset(dat,grp==5)
#dim(dat5)
#summary(dat5)
#dat6=subset(dat,grp==6)
#dim(dat6)
#summary(dat6)
#dat7=subset(dat,grp==7)
#dim(dat7)
#summary(dat7)
























