# STEP 1
# DATA MANIPULATION
# LOAD THE DATA 
# VISULAITATION
# MISSINGNESS
# COMPLETE CASES
data = read.csv("final.csv", header = TRUE, sep=";")
# dimensions of data
data=data[data$height_gprd>1,]
data=data[data$bmi_gprd>10,]
data=data[data$FEV1_FVC_ratio_gprd>5,]
data=data[data$FEV1_FVC_ratio_gprd<120,]
data=data[data$FEV1_exp_gprd<140,]
data=data[data$FVC_exp_gprd<140,]
data=data[data$FEV1_gprd>0.1,]
data=data[data$FVC_gprd>0.1,]
data=data[data$eosinophils_gprd>0.0001,]
dim(data)
#n= 218.691 individuals and m=85 variables 
#summary of data
head(data)
#drop practicians details and demographics marital status etc, see following variables;
#patid pracid realyob mob marital famnum chsreg chsdate prescr capsup ses frd crd regstat reggap internal tod
#toreason  deathdate  indexdate pracregion   prac_lcd   prac_uts
new = data[c(-1,-2)]
a=2:22
newdata=new[-a]
head(newdata)
# drop drugs mMRC scale ethnicity and neutrophils as physicians suggested
drops=c("drug_AZITHRO","drug_MONTELUKAST","drug_THEOPH","drug_OXYG","influenza","neutrophils_gprd","ethnic_gprd", "isblack_gprd")
dat=newdata[ , !(names(newdata) %in% drops)]
dim(dat)
# we exclude 32 variables 
# we know have n=218.691 and m=53 variables

# select the COPD patients that smoke
dat2=dat[dat$smoke_gprd!=0,]
dim(dat2)
dat3=dat2[dat2$smoke_gprd!=1,]
dim(dat3)
# we exclude n=38403 non smokers

# identify missing values
x=na.omit(dat3)
sapply(dat3,function(x) sum(is.na(x)))
# in total of n= 180.288 we have  eosinophils_gprd= 76859, FEV1_gprd= 73978 , FEV1_exp_gprd= 160.887, 
# FVC_gprd=  105.827, FVC_exp_gprd=  170829,FEV1_FVC_ratio_gprd=85759, missing values
# missingness in continue variables is more than 50% and in fact the complete cases are n= 4215 cases
data1=dat3[complete.cases(dat3),]
dim(data1)
head(data1)

# continuous and discrete cases 
cont=data1[c(2,3,4,47,49,50,51,52,53)]
head(cont)
categ=data1[c(-2,-3,-4,-47,-49,-50,-51,-52,-53)]
head(categ)
dim(categ)

# Explonatory analysis in continious variables
desc_stats <- data.frame(
  Min = apply(cont, 2, min), # minimum
  Med = apply(cont, 2, median), # median
  Mean = apply(cont, 2, mean), # mean
  SD = apply(cont, 2, sd), # Standard deviation
  Max = apply(cont, 2, max) # Maximum
)
desc_stats <- round(desc_stats, 6)
desc_stats


ggplot(data = cont) +
  geom_histogram(aes(x = age_at_entry), binwidth = 0.8)

# take the logarithm in BMI
ggplot(data = cont) +
  geom_histogram(aes(x = log(bmi_gprd)), binwidth = 0.1)
ggplot(data = cont) +
  geom_histogram(aes(x = height_gprd), binwidth = 0.035)
ggplot(data = cont) +
  geom_histogram(aes(x = eosinophils_gprd), binwidth = 0.2)
ggplot(data = cont) +
  geom_histogram(aes(x = FEV1_gprd), binwidth = 0.2)
ggplot(data = cont) +
  geom_histogram(aes(x = FVC_gprd), binwidth = 0.2)
ggplot(data = cont) +
  geom_histogram(aes(x = FVC_exp_gprd), binwidth = 0.2)
ggplot(data = cont) +
  geom_histogram(aes(x = FEV1_exp_gprd), binwidth = 0.2)
ggplot(data = cont) +
  geom_histogram(aes(x = FEV1_FVC_ratio_gprd), binwidth = 0.2)

# categorical variables
categ$atopy_gprd[categ$atopy_gprd==2]=3
categ$gerd_gprd[categ$gerd==2]=3
categ$hf_gprd[categ$hf_gprd==1]=0
categ$hf_gprd[categ$hf_gprd==6]=3
categ$hf_gprd[categ$hf_gprd==2]=3

head(categ)
cats = apply(categ, 2, function(x) nlevels(as.factor(x)))
categ=lapply(categ , factor)
cat=as.data.frame(categ)
dim(cat)
summary(cat)
# calculation of percentacies
for (i in 1:45) {
  print(list(i,prop.table(table(cat[,i]))))
}

# new variables
cat$Rhinitis_CRS= rep(0,3800)
cat$Rhinitis_CRS[cat$rhinitis_gprd==3 | cat$CRS_gprd==3] = 1
prop.table(table(cat$Rhinitis_CRS))
cat$Hypertension=rep(0,3800)
cat$Hypertension[cat$hypertensive_disease_gprd==3|cat$bp_hypertensive_disease_gprd==3]=1
prop.table(table(cat$Hypertension,cat$gender))
cat$Ischaemic=rep(0,3800)
cat$Ischaemic[cat$ihd_ischaemic_heart_disease_gprd==3|cat$ischaemic_heart_disease_gprd==3]=1
prop.table(table(cat$Ischaemic))
cat$Any_Heart_D=rep(0,3800)
cat$Any_Heart_D[cat$cardiac_disease_gprd==3|cat$heart_disease_gprd==3|cat$heart_disease_pulmonary_gprd==3|cat$other_forms_heart_disease_gprd==3]=1
prop.table(table(cat$Any_Heart_D))


keep_cat=c("gender","Asthma","Rhinitis_CRS", "Hypertension", "Ischaemic", "Any_Heart_D","drug_ICS", "drug_LABA" ,"drug_LABA_ICS", "drug_LAMA", "drug_MUCOLYTICS", "drug_OCS", "drug_SABA","drug_SAMA","atopy_gprd", "gerd_gprd","cvd_gprd", "depression_gprd", "anxiety_gprd","circulatory_sys_diseases_gprd","hf_gprd","mrc_breath_scale_gprd")
#keep_cat=c("gender","Asthma", "Hypertension", "Ischaemic", "Any_Heart_D","drug_ICS", "drug_LABA" ,"drug_LABA_ICS", "drug_LAMA", "drug_OCS", "drug_SAMA","atopy_gprd", 
 "depression_gprd", "anxiety_gprd","circulatory_sys_diseases_gprd","hf_gprd")
CAT=cat[ , (names(cat) %in% keep_cat)]

#keep_drug=c("drug_ICS", "drug_LABA" ,"drug_LABA_ICS", "drug_LAMA",  "drug_OCS", "drug_SAMA")
keep_drug=c("drug_ICS", "drug_LABA" ,"drug_LABA_ICS", "drug_LAMA", "drug_MUCOLYTICS", "drug_OCS", "drug_SABA","drug_SAMA")
DRUGS=CAT[ , (names(CAT) %in% keep_drug)]
dim(DRUGS)
COMOR= CAT[ , !(names(CAT) %in% keep_drug)]

dim(COMOR)
dru = apply(DRUGS, 2, function(x) nlevels(as.factor(x)))
drugs=lapply(DRUGS , factor)
drugs=as.data.frame(drugs)
dim(drugs)
summary(drugs)
dev.off()
par(mfrow=c(2,1))
for (i in 1:ncol(drugs)) {
  plot(drugs[,i], main=colnames(drugs)[i],
       ylab = "Count", col="pink", las = 1)
}



com=lapply(COMOR , factor)
como=as.data.frame(com)
dim(como)
summary(como)

dim(como)
summary(como)
dev.off()
par(mfrow=c(2,1))
for (i in 1:ncol(como)) {
  plot(como[,i], main=colnames(como)[i],
       ylab = "Count", col="blue", las = 1)
}

FINAL= cbind(como,cont,drugs)
head(FINAL)
dim(FINAL)
FINAL$GOLD= rep(0,3800)
FINAL$GOLD[FINAL$FEV1_exp_gprd>=80 & FINAL$FEV1_FVC_ratio_gprd<71] = 1
FINAL$GOLD[FINAL$FEV1_exp_gprd<80 & FINAL$FEV1_exp_gprd>=50& FINAL$FEV1_FVC_ratio_gprd<71] = 2
FINAL$GOLD[FINAL$FEV1_exp_gprd<50 & FINAL$FEV1_exp_gprd>=30& FINAL$FEV1_FVC_ratio_gprd<71] = 3
FINAL$GOLD[FINAL$FEV1_exp_gprd<30& FINAL$FEV1_FVC_ratio_gprd<71] = 4
FINAL$GOLD=as.factor(FINAL$GOLD)
summary(FINAL)
