# STEP 2 : MCA
dim(drugs)
head(drugs)
res.mca <- MCA(drugs,ncp=8, graph=FALSE)
explor(res.mca)
#1	25.0	25.0
#2	13.4	38.4
#3	12.5	50.9
#4	11.2	62.1
#5	11.2	73.3
#6	9.7	83.0
#7	8.9	91.9
#8	8.1	100.0
#first 5 component expain 73.3 of variance
ind <- get_mca_ind(res.mca)
head(ind)
head(ind$coord)
drug=res.mca$ind$coord
summary(res.mca, nb.dec = 2, ncp = 8)
dev.off()

# The plot above helps to identify variables that are the most 
# correlated with each dimension. The squared correlations between variables and the dimensions are used as coordinates.

# It can be seen that, the drugs mucolytics and laba lama are the most correlated with dimension 1. Similarly, saba sama sama are the most correlated with dimension 2.
# saba contributes the same
var= get_mca_var(res.mca)
var
# coordinates
head(round(var$coord, 2))
# contribution
contr=round(var$contrib,2)
(round(var$contrib,2))
plot(res.mca, choix = "var")
categories =rownames(var$coord)
length(categories)
print(categories)
library("corrplot")
corrplot(var$contrib, is.corr = FALSE)
# Contributions of variables on Dim.1
fviz_contrib(res.mca, choice = "var", axes = 1)
# Control category point colors using their contribution
# Possible values for the argument col.row are :
# "cos2", "contrib", "coord", "x", "y"
fviz_mca_var(res.mca, col.var = "contrib")

library("corrplot")
corrplot(var$cos2, is.corr=FALSE)
#[1]:http://www.sthda.com/english/wiki/multiple-correspondence-analysis-essentials-interpretation-and-application-to-investigate-the-associations-between-categories-of-multiple-qualitative-variables-r-software-and-data-mining
fviz_contrib(res.mca, choice ="ind", axes = 1, top = 20)
fviz_screeplot(res.mca)


como1=como[,-9]
dim(como1)
head(como1)

summary(como1)
res.mca <- MCA(como1,ncp=13, graph=FALSE)
explor(res.mca)
#1	15.3	15.3
#2	10.5	25.8
#3	9.1	34.9
#4	8.8	43.7
#5	7.6	51.4
#6	7.6	59.0
#7	7.3	66.3
#8	6.9	73.2
#9	6.7	79.9
#10	6.4	86.3
#11	5.8	92.1
#12	5.4	97.5
#13	2.5	100.0
# fis=rst 8 components explain 73.2%
ind <- get_mca_ind(res.mca)
head(ind)
head(ind$coord)
comorbid=res.mca$ind$coord
summary(res.mca, nb.dec = 2, ncp = 2)
dev.off()

# The plot above helps to identify variables that are the most 
# correlated with each dimension. The squared correlations between variables and the dimensions are used as coordinates.

# It can be seen that, the drugs mucolytics and laba lama are the most correlated with dimension 1. Similarly, saba sama sama are the most correlated with dimension 2.
# saba contributes the same
var= get_mca_var(res.mca)
var
# coordinates
head(round(var$coord, 2))
# contribution
contr=round(var$contrib,2)
(round(var$contrib,2))
plot(res.mca, choix = "var")
categories =rownames(var$coord)
length(categories)
print(categories)
library("corrplot")
corrplot(var$contrib, is.corr = FALSE)
# Contributions of variables on Dim.1
fviz_contrib(res.mca, choice = "var", axes = 1)
# Control category point colors using their contribution
# Possible values for the argument col.row are :
# "cos2", "contrib", "coord", "x", "y"
fviz_mca_var(res.mca, col.var = "contrib")

library("corrplot")
corrplot(var$cos2, is.corr=FALSE)
#[1]:http://www.sthda.com/english/wiki/multiple-correspondence-analysis-essentials-interpretation-and-application-to-investigate-the-associations-between-categories-of-multiple-qualitative-variables-r-software-and-data-mining
fviz_contrib(res.mca, choice ="ind", axes = 1, top = 20)
fviz_screeplot(res.mca)

