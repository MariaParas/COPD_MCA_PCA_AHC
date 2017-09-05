# STEP 2 : MCA
dim(drugs)
head(drugs)
res.mca <- MCA(drugs,ncp=8, graph=FALSE)
explor(res.mca)

ind <- get_mca_ind(res.mca)
head(ind)
head(ind$coord)
drug=res.mca$ind$coord
summary(res.mca, nb.dec = 2, ncp = 8)
dev.off()

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

ind <- get_mca_ind(res.mca)
head(ind)
head(ind$coord)
comorbid=res.mca$ind$coord
summary(res.mca, nb.dec = 2, ncp = 2)
dev.off()

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
fviz_mca_var(res.mca, col.var = "contrib")

library("corrplot")
corrplot(var$cos2, is.corr=FALSE)
#[1]:http://www.sthda.com/english/wiki/multiple-correspondence-analysis-essentials-interpretation-and-application-to-investigate-the-associations-between-categories-of-multiple-qualitative-variables-r-software-and-data-mining
fviz_contrib(res.mca, choice ="ind", axes = 1, top = 20)
fviz_screeplot(res.mca)

