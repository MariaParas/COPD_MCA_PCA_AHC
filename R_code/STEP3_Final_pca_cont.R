# STEP 3: PCA
cor.mat <- round(cor(cont),2)
head(cor.mat)
library("corrplot")
corrplot(cor.mat, type="upper", order="hclust", 
         tl.col="black", tl.srt=45)

chart.Correlation(cont, histogram=TRUE, pch=19)
res.pca <- PCA(cont,ncp=9,scale=TRUE, graph = FALSE)
print(res.pca)
eigenvalues <- res.pca$eig
head(eigenvalues[, 1:2])
barplot(eigenvalues[, 2], names.arg=1:nrow(eigenvalues), 
        main = "Variances",
        xlab = "Principal Components",
        ylab = "Percentage of variances",
        col ="steelblue")
lines(x = 1:nrow(eigenvalues), eigenvalues[, 2], 
      type="b", pch=19, col = "red")
fviz_screeplot(res.pca, ncp=10)
head(res.pca$var$coord)
head(res.pca$var$cos2)
head(res.pca$var$contrib)
plot(res.pca, choix = "var")
fviz_pca_var(res.pca)
fviz_pca_var(res.pca, col.var="contrib")
fviz_pca_ind(res.pca)
fviz_pca_ind(res.pca, label="none")
fviz_pca_contrib(res.pca, choice = "var", axes = 1)
fviz_pca_contrib(res.pca, choice = "var", axes = 2)
res.desc <- dimdesc(res.pca, axes = c(1,2))
res.desc$Dim.1
res.desc$Dim.2


explor(res.pca)

ind <- get_pca_ind(res.pca)
head(ind)
head(ind$coord)
cont_p=res.pca$ind$coord
summary(res.pca, nb.dec = 2, ncp = 2)
dev.off()
var= get_pca_var(res.pca)
var
# coordinates
head(round(var$coord, 2))
# contribution
contr=round(var$contrib,2)
head(round(var$contrib,2))
plot(res.pca, choix = "var")
categories =rownames(var$coord)
length(categories)
print(categories)
library("corrplot")
corrplot(var$contrib, is.corr = FALSE)
# Contributions of variables on Dim.1
fviz_mca_var(res.pca, col.var = "contrib")

library("corrplot")
corrplot(var$cos2, is.corr=FALSE)
#[1]:http://www.sthda.com/english/wiki/multiple-correspondence-analysis-essentials-interpretation-and-application-to-investigate-the-associations-between-categories-of-multiple-qualitative-variables-r-software-and-data-mining
fviz_contrib(res.pca, choice ="ind", axes = 1, top = 20)
fviz_screeplot(res.pca)









