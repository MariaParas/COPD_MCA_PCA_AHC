# STEP 6: Chi square test of independence and ANOVA tests for cheking differences between clusters
# categorical variables
tbl = table(dat$grp,dat$atopy_gprd) 
chisq.test(tbl)

tbl = table(dat$grp,dat$gender) 
chisq.test(tbl)

tbl = table(dat$grp,dat$gerd_gprd) 
chisq.test(tbl)

tbl = table(dat$grp,dat$depression_gprd) 
chisq.test(tbl)

tbl = table(dat$grp,dat$anxiety) 
chisq.test(tbl)

tbl = table(dat$grp,dat$circulatory_sys_diseases_gprd) 
chisq.test(tbl)

tbl = table(dat$grp,dat$cvd_gprd) 
chisq.test(tbl)

tbl = table(dat$grp,dat$hf_gprd) 
chisq.test(tbl)


tbl = table(dat$grp,dat$mrc_breath_scale_gprd) 
chisq.test(tbl)

tbl = table(dat$grp,dat$Asthma) 
chisq.test(tbl)

tbl = table(dat$grp,dat$Rhinitis_CRS) 
chisq.test(tbl)


tbl = table(dat$grp,dat$Hypertension) 
chisq.test(tbl)

tbl = table(dat$grp,dat$Ischaemic) 
chisq.test(tbl)

tbl = table(dat$grp,dat$Any_Heart_D) 
chisq.test(tbl)
# continuous variables

summary(lm(dat$FVC_gprd~as.factor(dat$grp)))
summary(lm(dat$FEV1_exp_gprd~as.factor(dat$grp)))

summary(lm(dat$eosinophils_gprd~as.factor(dat$grp)))
summary(lm(dat$FEV1_FVC_ratio_gprd~as.factor(dat$grp)))

summary(lm(dat$height_gprd~as.factor(dat$grp)))
summary(lm(dat$bmi_gprd~as.factor(dat$grp)))
summary(lm(dat$age_at_entry~as.factor(dat$grp)))
