#NOTE: Predicted FCV values based on British Thoracic Society article
#      for individuals aged >=38 years old
#      we have exclude cases where height takes values in the interval [0,100] cm
#setwd("/Volumes/grp11$/FPHS_Farr_MSc_MachineLearning/copd_small")
setwd("S:/FPHS_Farr_MSc_MachineLearning/copd_small")
data = read.csv("final.csv", header = TRUE, sep=";")
head(data)
length(data)
str(data)
dim(data)
complete.cases(data)
x=na.omit(data)
sapply(data,function(x) sum(is.na(x)))
data1=data[data$height_gprd>0.1,]
data1$FVC_gprd
sapply(data1,function(x) sum(is.na(x)))
data2=data1[data1$age_at_entry>30,]
sapply(data2,function(x) sum(is.na(x)))

hist(data2$age_at_entry,
     main = "Histogram of Age",
     xlab = "Age in Years")
summary(data2$age_at_entry)
d = density(data2$age_at_entry)
plot(d, main = "Kernel density of Age")
polygon(d, col = "white", border = "black")


###MALES###
# 1. AGE [38,41] 
# HEIGHT < 1.65
# HEIGHT [1.65-1.70]
# HEIGHT [1.70-1.75]
# HEIGHT [1.75-1.80]
# HEIGHT [1.80-1.85]
# HEIGHT [1.85-1.90]
# HEIGHT >= 1.90

attach(data2)

data2$FCV_pred[(age_at_entry<42) & (gender=1) & (height_gprd<1.65)] = 3.81
data2$FCV_pred[(age_at_entry<42) & (gender=1) & (height_gprd>=1.65 & height_gprd<1.70)]= 4.10
data2$FCV_pred[(age_at_entry<42) & (gender=1) & (height_gprd>=1.70 & height_gprd<1.75)]= 4.39
data2$FCV_pred[(age_at_entry<42) & (gender=1) & (height_gprd>=1.75 & height_gprd<1.80)]= 4.67
data2$FCV_pred[(age_at_entry<42) & (gender=1) & (height_gprd>=1.80 & height_gprd<1.85)]= 4.96
data2$FCV_pred[(age_at_entry<42) & (gender=1) & (height_gprd>=1.85 & height_gprd<1.90)]= 5.25
data2$FCV_pred[(age_at_entry<42) & (gender=1) & (height_gprd>=1.90)]= 5.54
# AGE [42,45]
data2$FCV_pred[(age_at_entry>=42 & age_at_entry<46) & (gender=1) & (height_gprd<1.65)] = 3.71
data2$FCV_pred[(age_at_entry>=42 & age_at_entry<46) & (gender=1) & (height_gprd>=1.65 & height_gprd<1.70)]= 3.99
data2$FCV_pred[(age_at_entry>=42 & age_at_entry<46) & (gender=1) & (height_gprd>=1.70 & height_gprd<1.75)]= 4.28
data2$FCV_pred[(age_at_entry>=42 & age_at_entry<46) & (gender=1) & (height_gprd>=1.75 & height_gprd<1.80)]= 4.57
data2$FCV_pred[(age_at_entry>=42 & age_at_entry<46) & (gender=1) & (height_gprd>=1.80 & height_gprd<1.85)]= 4.86
data2$FCV_pred[(age_at_entry>=42 & age_at_entry<46) & (gender=1) & (height_gprd>=1.85 & height_gprd<1.90)]= 5.15
data2$FCV_pred[(age_at_entry>=42 & age_at_entry<46) & (gender=1) & (height_gprd>=1.90)]= 5.43

# AGE [46,49]
data2$FCV_pred[(age_at_entry>=46 & age_at_entry<50) & (gender=1) & (height_gprd<1.65)] = 3.60
data2$FCV_pred[(age_at_entry>=46 & age_at_entry<50) & (gender=1) & (height_gprd>=1.65 & height_gprd<1.70)]= 3.89
data2$FCV_pred[(age_at_entry>=46 & age_at_entry<50) & (gender=1) & (height_gprd>=1.70 & height_gprd<1.75)]= 4.18
data2$FCV_pred[(age_at_entry>=46 & age_at_entry<50) & (gender=1) & (height_gprd>=1.75 & height_gprd<1.80)]= 4.47
data2$FCV_pred[(age_at_entry>=46 & age_at_entry<50) & (gender=1) & (height_gprd>=1.80 & height_gprd<1.85)]= 4.75
data2$FCV_pred[(age_at_entry>=46 & age_at_entry<50) & (gender=1) & (height_gprd>=1.85 & height_gprd<1.90)]= 5.04
data2$FCV_pred[(age_at_entry>=46 & age_at_entry<50) & (gender=1) & (height_gprd>=1.90)]= 5.33

# AGE [50,53]
data2$FCV_pred[(age_at_entry>=50 & age_at_entry<54) & (gender=1) & (height_gprd<1.65)] = 3.50
data2$FCV_pred[(age_at_entry>=50 & age_at_entry<54) & (gender=1) & (height_gprd>=1.65 & height_gprd<1.70)]= 3.79
data2$FCV_pred[(age_at_entry>=50 & age_at_entry<54) & (gender=1) & (height_gprd>=1.70 & height_gprd<1.75)]= 4.07
data2$FCV_pred[(age_at_entry>=50 & age_at_entry<54) & (gender=1) & (height_gprd>=1.75 & height_gprd<1.80)]= 4.36
data2$FCV_pred[(age_at_entry>=50 & age_at_entry<54) & (gender=1) & (height_gprd>=1.80 & height_gprd<1.85)]= 4.65
data2$FCV_pred[(age_at_entry>=50 & age_at_entry<54) & (gender=1) & (height_gprd>=1.85 & height_gprd<1.90)]= 4.94
data2$FCV_pred[(age_at_entry>=50 & age_at_entry<54) & (gender=1) & (height_gprd>=1.90)]= 5.23

# AGE [54,57]
data2$FCV_pred[(age_at_entry>=54 & age_at_entry<58) & (gender=1) & (height_gprd<1.65)] = 3.39
data2$FCV_pred[(age_at_entry>=54 & age_at_entry<58) & (gender=1) & (height_gprd>=1.65 & height_gprd<1.70)]= 3.68
data2$FCV_pred[(age_at_entry>=54 & age_at_entry<58) & (gender=1) & (height_gprd>=1.70 & height_gprd<1.75)]= 3.97
data2$FCV_pred[(age_at_entry>=54 & age_at_entry<58) & (gender=1) & (height_gprd>=1.75 & height_gprd<1.80)]= 4.26
data2$FCV_pred[(age_at_entry>=54 & age_at_entry<58) & (gender=1) & (height_gprd>=1.80 & height_gprd<1.85)]= 4.55
data2$FCV_pred[(age_at_entry>=54 & age_at_entry<58) & (gender=1) & (height_gprd>=1.85 & height_gprd<1.90)]= 5.83
data2$FCV_pred[(age_at_entry>=54 & age_at_entry<58) & (gender=1) & (height_gprd>=1.90)]= 5.12

# AGE [58,61]
data2$FCV_pred[(age_at_entry>=58 & age_at_entry<62) & (gender=1) & (height_gprd<1.65)] = 3.29
data2$FCV_pred[(age_at_entry>=58 & age_at_entry<62) & (gender=1) & (height_gprd>=1.65 & height_gprd<1.70)]= 3.58
data2$FCV_pred[(age_at_entry>=58 & age_at_entry<62) & (gender=1) & (height_gprd>=1.70 & height_gprd<1.75)]= 3.87
data2$FCV_pred[(age_at_entry>=58 & age_at_entry<62) & (gender=1) & (height_gprd>=1.75 & height_gprd<1.80)]= 4.15
data2$FCV_pred[(age_at_entry>=58 & age_at_entry<62) & (gender=1) & (height_gprd>=1.80 & height_gprd<1.85)]= 4.44
data2$FCV_pred[(age_at_entry>=58 & age_at_entry<62) & (gender=1) & (height_gprd>=1.85 & height_gprd<1.90)]= 4.73
data2$FCV_pred[(age_at_entry>=58 & age_at_entry<62) & (gender=1) & (height_gprd>=1.90)]= 5.02

# AGE [62,65]
data2$FCV_pred[(age_at_entry>=62 & age_at_entry<66) & (gender=1) & (height_gprd<1.65)] = 3.19
data2$FCV_pred[(age_at_entry>=62 & age_at_entry<66) & (gender=1) & (height_gprd>=1.65 & height_gprd<1.70)]= 3.47
data2$FCV_pred[(age_at_entry>=62 & age_at_entry<66) & (gender=1) & (height_gprd>=1.70 & height_gprd<1.75)]= 3.76
data2$FCV_pred[(age_at_entry>=62 & age_at_entry<66) & (gender=1) & (height_gprd>=1.75 & height_gprd<1.80)]= 4.05
data2$FCV_pred[(age_at_entry>=62 & age_at_entry<66) & (gender=1) & (height_gprd>=1.80 & height_gprd<1.85)]= 4.34
data2$FCV_pred[(age_at_entry>=62 & age_at_entry<66) & (gender=1) & (height_gprd>=1.85 & height_gprd<1.90)]= 4.63
data2$FCV_pred[(age_at_entry>=62 & age_at_entry<66) & (gender=1) & (height_gprd>=1.90)]= 4.91

# AGE [66, 69]
data2$FCV_pred[(age_at_entry>=66 & age_at_entry<70) & (gender=1) & (height_gprd<1.65)] = 3.08
data2$FCV_pred[(age_at_entry>=66 & age_at_entry<70) & (gender=1) & (height_gprd>=1.65 & height_gprd<1.70)]= 3.37
data2$FCV_pred[(age_at_entry>=66 & age_at_entry<70) & (gender=1) & (height_gprd>=1.70 & height_gprd<1.75)]= 3.66
data2$FCV_pred[(age_at_entry>=66 & age_at_entry<70) & (gender=1) & (height_gprd>=1.75 & height_gprd<1.80)]= 3.95
data2$FCV_pred[(age_at_entry>=66 & age_at_entry<70) & (gender=1) & (height_gprd>=1.80 & height_gprd<1.85)]= 4.23
data2$FCV_pred[(age_at_entry>=66 & age_at_entry<70) & (gender=1) & (height_gprd>=1.85 & height_gprd<1.90)]= 4.52
data2$FCV_pred[(age_at_entry>=66 & age_at_entry<70) & (gender=1) & (height_gprd>=1.90)]= 4.81
# AGE [70,...]
data2$FCV_pred[(age_at_entry>=70) & (gender=1)]=100*0.0576*data2$height_gprd[(age_at_entry>=70) & (gender=1)]-0.026*data2$age_at_entry[(age_at_entry>=70) & (gender=1)]-4.34


###FEMALES###
# 1. AGE [38,41] 
# HEIGHT < 1.55
# HEIGHT [1.55-1.60]
# HEIGHT [1.60-1.65]
# HEIGHT [1.65-1.70]
# HEIGHT [1.70-1.75]
# HEIGHT [1.75-1.80]
# HEIGHT >= 1.80

data2$FCV_pred[(age_at_entry<42) & (gender=0) & (height_gprd<1.55)] = 2.69
data2$FCV_pred[(age_at_entry<42) & (gender=0) & (height_gprd>=1.55 & height_gprd<1.60)]= 2.91
data2$FCV_pred[(age_at_entry<42) & (gender=0) & (height_gprd>=1.60 & height_gprd<1.65)]= 3.13
data2$FCV_pred[(age_at_entry<42) & (gender=0) & (height_gprd>=1.65 & height_gprd<1.70)]= 3.35
data2$FCV_pred[(age_at_entry<42) & (gender=0) & (height_gprd>=1.70 & height_gprd<1.75)]= 3.58
data2$FCV_pred[(age_at_entry<42) & (gender=0) & (height_gprd>=1.75 & height_gprd<1.80)]= 3.80
data2$FCV_pred[(age_at_entry<42) & (gender=0) & (height_gprd>=1.80)]= 4.02
# AGE [42,45]
data2$FCV_pred[(age_at_entry>=42 & age_at_entry<46) & (gender=0) & (height_gprd<1.55)] = 2.59
data2$FCV_pred[(age_at_entry>=42 & age_at_entry<46) & (gender=0) & (height_gprd>=1.55 & height_gprd<1.60)]= 2.81
data2$FCV_pred[(age_at_entry>=42 & age_at_entry<46) & (gender=0) & (height_gprd>=1.60 & height_gprd<1.65)]= 3.03
data2$FCV_pred[(age_at_entry>=42 & age_at_entry<46) & (gender=0) & (height_gprd>=1.65 & height_gprd<1.70)]= 3.25
data2$FCV_pred[(age_at_entry>=42 & age_at_entry<46) & (gender=0) & (height_gprd>=1.70 & height_gprd<1.75)]= 3.47
data2$FCV_pred[(age_at_entry>=42 & age_at_entry<46) & (gender=0) & (height_gprd>=1.75 & height_gprd<1.80)]= 3.69
data2$FCV_pred[(age_at_entry>=42 & age_at_entry<46) & (gender=0) & (height_gprd>=1.80)]= 3.91

# AGE [46,49]
data2$FCV_pred[(age_at_entry>=46 & age_at_entry<50) & (gender=0) & (height_gprd<1.55)] = 2.48
data2$FCV_pred[(age_at_entry>=46 & age_at_entry<50) & (gender=0) & (height_gprd>=1.55 & height_gprd<1.60)]= 2.70
data2$FCV_pred[(age_at_entry>=46 & age_at_entry<50) & (gender=0) & (height_gprd>=1.60 & height_gprd<1.65)]= 2.92
data2$FCV_pred[(age_at_entry>=46 & age_at_entry<50) & (gender=0) & (height_gprd>=1.65 & height_gprd<1.70)]= 3.15
data2$FCV_pred[(age_at_entry>=46 & age_at_entry<50) & (gender=0) & (height_gprd>=1.70 & height_gprd<1.75)]= 3.37
data2$FCV_pred[(age_at_entry>=46 & age_at_entry<50) & (gender=0) & (height_gprd>=1.75 & height_gprd<1.80)]= 3.59
data2$FCV_pred[(age_at_entry>=46 & age_at_entry<50) & (gender=0) & (height_gprd>=1.80)]= 3.81

# AGE [50,53]
data2$FCV_pred[(age_at_entry>=50 & age_at_entry<54) & (gender=0) & (height_gprd<1.55)] = 2.38
data2$FCV_pred[(age_at_entry>=50 & age_at_entry<54) & (gender=0) & (height_gprd>=1.55 & height_gprd<1.60)]= 2.60
data2$FCV_pred[(age_at_entry>=50 & age_at_entry<54) & (gender=0) & (height_gprd>=1.60 & height_gprd<1.65)]= 2.82
data2$FCV_pred[(age_at_entry>=50 & age_at_entry<54) & (gender=0) & (height_gprd>=1.65 & height_gprd<1.70)]= 3.04
data2$FCV_pred[(age_at_entry>=50 & age_at_entry<54) & (gender=0) & (height_gprd>=1.70 & height_gprd<1.75)]= 3.26
data2$FCV_pred[(age_at_entry>=50 & age_at_entry<54) & (gender=0) & (height_gprd>=1.75 & height_gprd<1.80)]= 3.48
data2$FCV_pred[(age_at_entry>=50 & age_at_entry<54) & (gender=0) & (height_gprd>=1.80)]= 3.71

# AGE [54,57]
data2$FCV_pred[(age_at_entry>=54 & age_at_entry<58) & (gender=0) & (height_gprd<1.55)] = 2.27
data2$FCV_pred[(age_at_entry>=54 & age_at_entry<58) & (gender=0) & (height_gprd>=1.55 & height_gprd<1.60)]= 2.49
data2$FCV_pred[(age_at_entry>=54 & age_at_entry<58) & (gender=0) & (height_gprd>=1.60 & height_gprd<1.65)]= 2.72
data2$FCV_pred[(age_at_entry>=54 & age_at_entry<58) & (gender=0) & (height_gprd>=1.65 & height_gprd<1.70)]= 2.94
data2$FCV_pred[(age_at_entry>=54 & age_at_entry<58) & (gender=0) & (height_gprd>=1.70 & height_gprd<1.75)]= 3.16
data2$FCV_pred[(age_at_entry>=54 & age_at_entry<58) & (gender=0) & (height_gprd>=1.75 & height_gprd<1.80)]= 3.38
data2$FCV_pred[(age_at_entry>=54 & age_at_entry<58) & (gender=0) & (height_gprd>=1.80)]= 3.60

# AGE [58,61]
data2$FCV_pred[(age_at_entry>=58 & age_at_entry<62) & (gender=0) & (height_gprd<1.55)] = 2.17
data2$FCV_pred[(age_at_entry>=58 & age_at_entry<62) & (gender=0) & (height_gprd>=1.55 & height_gprd<1.60)]= 2.39
data2$FCV_pred[(age_at_entry>=58 & age_at_entry<62) & (gender=0) & (height_gprd>=1.60 & height_gprd<1.65)]= 2.61
data2$FCV_pred[(age_at_entry>=58 & age_at_entry<62) & (gender=0) & (height_gprd>=1.65 & height_gprd<1.70)]= 2.83
data2$FCV_pred[(age_at_entry>=58 & age_at_entry<62) & (gender=0) & (height_gprd>=1.70 & height_gprd<1.75)]= 3.06
data2$FCV_pred[(age_at_entry>=58 & age_at_entry<62) & (gender=0) & (height_gprd>=1.75 & height_gprd<1.80)]= 3.28
data2$FCV_pred[(age_at_entry>=58 & age_at_entry<62) & (gender=0) & (height_gprd>=1.80)]= 3.50

# AGE [62,65]
data2$FCV_pred[(age_at_entry>=62 & age_at_entry<66) & (gender=0) & (height_gprd<1.55)] = 2.07
data2$FCV_pred[(age_at_entry>=62 & age_at_entry<66) & (gender=0) & (height_gprd>=1.55 & height_gprd<1.60)]= 2.29
data2$FCV_pred[(age_at_entry>=62 & age_at_entry<66) & (gender=0) & (height_gprd>=1.60 & height_gprd<1.65)]= 2.51
data2$FCV_pred[(age_at_entry>=62 & age_at_entry<66) & (gender=0) & (height_gprd>=1.65 & height_gprd<1.70)]= 2.73
data2$FCV_pred[(age_at_entry>=62 & age_at_entry<66) & (gender=0) & (height_gprd>=1.70 & height_gprd<1.75)]= 2.95
data2$FCV_pred[(age_at_entry>=62 & age_at_entry<66) & (gender=0) & (height_gprd>=1.75 & height_gprd<1.80)]= 3.17
data2$FCV_pred[(age_at_entry>=62 & age_at_entry<66) & (gender=0) & (height_gprd>=1.80)]= 3.39

# AGE [66, 69]
data2$FCV_pred[(age_at_entry>=66 & age_at_entry<70) & (gender=0) & (height_gprd<1.55)] = 1.96
data2$FCV_pred[(age_at_entry>=66 & age_at_entry<70) & (gender=0) & (height_gprd>=1.55 & height_gprd<1.60)]= 2.18
data2$FCV_pred[(age_at_entry>=66 & age_at_entry<70) & (gender=0) & (height_gprd>=1.60 & height_gprd<1.65)]= 2.40
data2$FCV_pred[(age_at_entry>=66 & age_at_entry<70) & (gender=0) & (height_gprd>=1.65 & height_gprd<1.70)]= 2.63
data2$FCV_pred[(age_at_entry>=66 & age_at_entry<70) & (gender=0) & (height_gprd>=1.70 & height_gprd<1.75)]= 2.85
data2$FCV_pred[(age_at_entry>=66 & age_at_entry<70) & (gender=0) & (height_gprd>=1.75 & height_gprd<1.80)]= 3.07
data2$FCV_pred[(age_at_entry>=66 & age_at_entry<70) & (gender=0) & (height_gprd>=1.80)]= 3.29
# AGE [70,...]
data2$FCV_pred[(age_at_entry>=70) & (gender=0)]=100*0.0443*data2$height_gprd[(age_at_entry>=70) & (gender=0)]-0.026*data2$age_at_entry[(age_at_entry>=70) & (gender=0)]-2.89

data2$FCV_pred

hist(data2$FCV_pred , main="Histogram  of FCV_pred")
abline(v=mean(data2$FCV_pred), col= "blue")

abline(v=median(data2$FCV_pred), col = "green")

legend("topright", c("Mean ", "Median"), pch = 16,col= c("blue", "green"))

# there are some negative values
c=which(data2$FCV_pred<0)
c=as.vector(c)
length(c)
bad=data2[c,]
# look them closely
# most of them very small high less than 100 cm
bad_h=cbind(bad$height_gprd,bad$age_at_entry,bad$gender,bad$FCV_pred)

# exclude them
data2=data2[-c,]

hist(data2$FCV_pred , main="Histogram  of FCV_pred")
abline(v=mean(data2$FCV_pred), col= "blue")

abline(v=median(data2$FCV_pred), col = "green")

legend("topright", c("Mean ", "Median"), pch = 16,col= c("blue", "green"))



#ggplot2.histogram(data=data2, xName='FCV_pred',
#                  groupName='gender', legendPosition="top",
#                  alpha=0.05, addDensity=TRUE,
#                  addMeanLine=TRUE, meanLineColor="white", meanLineSize=1)


# reduce by 13% the FCV_pred if isblack_gprd is 1

#for (i in length(data2$FCV_pred)){
#  if (isblack_gprd[i]==1){
#    data2$FCV_pred[i]= data2$FCV_pred[i] - data2$FCV_pred[i]*0.13
#  }
#}


#output=cbind(data2$patid,data2$FCV_pred)
#colnames(output) = c('Id','FCV_pred')
#write.csv(output,'FCV_pred',row.names=FALSE)


ggplot2.histogram(data=data2, xName='FCV_pred',
                  groupName='gender', legendPosition="top",
                  alpha=0.05, addDensity=TRUE,
                  addMeanLine=TRUE, meanLineColor="white", meanLineSize=1)

ggplot2.histogram(data=data2, xName='FCV_pred',
                  groupName='isblack_gprd', legendPosition="top",
                  alpha=0.05, addDensity=TRUE,
                  addMeanLine=TRUE, meanLineColor="white", meanLineSize=1)

# look for missingness again and replace the missing values in FCV 
# total number of observations 232074
x=na.omit(data2)

sapply(data2,function(x) sum(is.na(x)))

length(data2$FCV_pred)
summary(data2$FCV_pred)
summary(data2$FVC_gprd)
# replace the missing values in the FVC_gprd

#c=is.na(data2$FVC_gprd)
#a= which(c==TRUE)
#data2$FVC_gprd[a]=data2$FCV_pred[a]

#FVC_gPr=rep(0,length(data2$FCV_pred))
#for (i in length(data2$FCV_pred)){
#if (c[i]==TRUE){
#FVC_gPr[i]== data2$FCV_pred[i] 
#}
#else{
#FVC_gPr[i]== data2$FVC_gprd[i]
#}
#}


#ggplot2.histogram(data=data2, xName='FVC_gprd',
#                  groupName='gender', legendPosition="top",
#                  alpha=0.05, addDensity=TRUE,
#                  addMeanLine=TRUE, meanLineColor="white", meanLineSize=1)

#ggplot2.histogram(data=data2, xName='FVC_gprd',
#                  groupName='isblack_gprd', legendPosition="top",
#                  alpha=0.05, addDensity=TRUE,
#                  addMeanLine=TRUE, meanLineColor="white", meanLineSize=1)

# compare FVC_gprd and FCV_pred by using a t- test

#b=which(c==FALSE)
#t.test(data2$FVC_gprd[b],data2$FCV_pred[b])

# the difference is not statistically significant p-value<5%
# henceforth we dont reject the null hypothesis for equality 



