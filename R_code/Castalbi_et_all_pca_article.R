setwd("S:/FPHS_Farr_MSc_MachineLearning/copd_small")
data = read.csv("final.csv", header = TRUE, sep=";")
data$mrc_breath_scale_gprd
length(data)
str(data)
dim(data)
complete.cases(data)
x=na.omit(data)
sapply(data,function(x) sum(is.na(x)))
data0=data[data$height_gprd>0.1,]
data0i=data0[data0$eosinophils_gprd>0,]
data0ii=data0i[data0i$FEV1_FVC_ratio_gprd>20,]
data0iii=data0ii[data0ii$FEV1_FVC_ratio_gprd<100,]
data0iiii=data0iii[data0iii$FEV1_exp_gprd<150,]
data0iiiii=data0iiii[data0iiii$FVC_exp_gprd<150,]
data1=data0iiiii[data0iiiii$bmi_gprd>5,]
dim(data1)
x=na.omit(data)
sapply(data1,function(x) sum(is.na(x)))
# exclude demographics and prectitians id etc
new <- data1[c(-1,-2)]
a=2:22
newdata=new[-a]
head(newdata)
drops=c("drug_AZITHRO","drug_MONTELUKAST","drug_THEOPH","drug_OXYG","influenza","neutrophils_gprd","ethnic_gprd", "isblack_gprd")
dat=newdata[ , !(names(newdata) %in% drops)]
dat2=dat[complete.cases(dat),]
keep_pca=c("FEV1_exp_gprd","FVC_exp_gprd","FEV1_FVC_ratio_gprd","mrc_breath_scale_gprd","bmi_gprd","Asthma","cvd_gprd")
pc=dat2[ , (names(dat2) %in% keep_pca)]
install.packages("dummies", dependencies = FALSE)
library(dummies)
pc$mrc_breath_scale_gprd[pc$mrc_breath_scale_gprd==5]=4
summary(pc)
#create a dummy data frame
new_my_data <- dummy.data.frame(pc, names = c("mrc_breath_scale_gprd","Asthma","cvd_gprd"))

prin_comp <- prcomp(new_my_data, scale. = F)
names(prin_comp)
# obtain the scores
prin_comp$rotation
#scree plot
screeplot(prin_comp)
# perform the bar plots
m=c("BMI","CVD-","CVD+","MRC0","MRC1","MRC2","MRC3","MRC4","FEV_Pr","FVC_Pr","FEV1/FVC","Asthma-","Asthma+")
sc1=prin_comp$rotation[,1:4]
PC1=sc1[,1]
names(PC1)=m
PC2=sc1[,2]
names(PC2)=m
PC3=sc1[,3]
names(PC3)=m
PC4=sc1[,4]
names(PC4)=m
dev.off()
par(mfrow=c(2,2))
barplot(PC1, las = 2,ylim=c(-1,1),main="PC1",ylab="scores")
barplot(PC2, las = 2,ylim=c(-1,1),main="PC2",ylab="scores")
barplot(PC3, las = 2,ylim=c(-1,1),main="PC3",ylab="scores")
barplot(PC4, las = 2,ylim=c(-1,1),main="PC4",ylab="scores")

