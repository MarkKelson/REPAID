##########
library(meta)
library(readxl)
library(tidyverse)

# Importing data ####
PAreview_initial_extractdat <- read_excel(path="/Users/markkelson/Dropbox/SysRev_Apps_PA/Code/PA sys rev data.xlsx",sheet=1)
PAsummarydat <- read_excel(path="/Users/markkelson/Dropbox/SysRev_Apps_PA/Code/Study_Summary_Info.xlsx",sheet=1)
GW4ReviewBCT <- read_excel(path="/Users/markkelson/Dropbox/SysRev_Apps_PA/Code/GW4 review BCT's.xlsx",sheet=2)

PAreviewdat_temp <- merge(PAreview_initial_extractdat,PAsummarydat,by.y ="Author")

PAreviewdat <- merge(PAreviewdat_temp,GW4ReviewBCT,by.y="Author")

PAreviewdat$GP_ind <- factor(PAreviewdat$GP_ind,labels=c("No","Yes"))
PAreviewdat$FM_ind <- factor(PAreviewdat$FM_ind,labels=c("No","Yes"))
PAreviewdat$SK_ind <- factor(PAreviewdat$SK_ind,labels=c("No","Yes"))
PAreviewdat$NC_ind <- factor(PAreviewdat$NC_ind,labels=c("No","Yes"))
PAreviewdat$CoB_ind <- factor(PAreviewdat$CoB_ind,labels=c("No","Yes"))
PAreviewdat$RaT_ind <- factor(PAreviewdat$RaT_ind,labels=c("No","Yes"))
PAreviewdat$Antec_ind <- factor(PAreviewdat$Antec_ind,labels=c("No","Yes"))




PAreviewdat <- rename(PAreviewdat, Goals_and_planning=GP_ind,
                      Feedback_and_monitoring=FM_ind,
                      Sharing_knowledge=SK_ind,
                      Natural_consequences=NC_ind,
                      Comparison_of_behaviour=CoB_ind,
                      Reward_and_threat=RaT_ind,
                      Antecedents=Antec_ind)

PAreviewdat$Me <- as.numeric(PAreviewdat$Me)

PAreviewdat$Followup_months_cat <- rep(NA,length(PAreviewdat$Followup_months))
PAreviewdat$Followup_months_cat[PAreviewdat$Followup_months<6] <- "<6"
PAreviewdat$Followup_months_cat[PAreviewdat$Followup_months>=6 & PAreviewdat$Followup_months <=12] <- "6-12"  
PAreviewdat$Followup_months_cat[PAreviewdat$Followup_months>12] <- ">12"


PArevQ1prim <- PAreviewdat[PAreviewdat$Question==1&PAreviewdat$Primary=="Y",]
PArevQ3prim <- PAreviewdat[PAreviewdat$Question==3&PAreviewdat$Primary=="Y",]


# Question Low SES ####

m1 <- metacont(data=PArevQ1prim,
               mean.e=Me,
               sd.e=Se,
               n.e=Ne,
               mean.c=Mc,
               sd.c=Sc,
               n.c=Nc,
               studlab=Author,
               sm="SMD",
               hakn=T)

forest(m1,digits.mean=1,digits.sd=1)


m1_sub_ses <- metacont(data=PArevQ1prim,
               mean.e=Me,
               sd.e=Se,
               n.e=Ne,
               mean.c=Mc,
               sd.c=Sc,
               n.c=Nc,
               studlab=Author,
               sm="SMD",
               hakn=T,byvar=SES)
m1_sub_ses


#tiff("/Users/markkelson/Dropbox/SysRev_Apps_PA/Code/Q1.tiff", units="in", width=11, height=11, res=200)
#png(file = '/Users/markkelson/Dropbox/SysRev_Apps_PA/Code/Q1.png',height=900,width=900) 
png(file = '/Users/markkelson/Dropbox/SysRev_Apps_PA/Code/Figure2.png',height=900,width=900) 
#pdf(file = '/Users/markkelson/Dropbox/SysRev_Apps_PA/Code/Figure2.pdf',width=13,height=11) 

forest(m1_sub_ses,digits.mean=1,digits.sd=1,subgroup=T,
       label.right = "Favours intervention",
       label.left = "Favours control")

dev.off() 

#Funnel plot
png(file = '/Users/markkelson/Dropbox/SysRev_Apps_PA/Code/Q1_funnel.png',height=900,width=900) 

funnel(m1)

dev.off()

metabias.meta(m1)

round(metabias.meta(m1)[[4]],2)


# Splitting by follow-up time
png(file = '/Users/markkelson/Dropbox/SysRev_Apps_PA/Code/Q1_time.png',height=900,width=900) 

m1_sub_time <- metacont(data=PArevQ1prim,
                       mean.e=Me,
                       sd.e=Se,
                       n.e=Ne,
                       mean.c=Mc,
                       sd.c=Sc,
                       n.c=Nc,
                       studlab=Author,
                       sm="SMD",
                       hakn=T,byvar=Followup_months_cat)
forest(m1_sub_time,digits.mean=1,digits.sd=1,subgroup=T)

dev.off()


# Question in low SES split by outcome

png(file = '/Users/markkelson/Dropbox/SysRev_Apps_PA/Code/Q1_outcome.png',height=1000,width=1000) 

m1_sub_Outcome <- metacont(data=PArevQ1prim,
                       mean.e=Me,
                       sd.e=Se,
                       n.e=Ne,
                       mean.c=Mc,
                       sd.c=Sc,
                       n.c=Nc,
                       studlab=Author,
                       sm="SMD",
                       hakn=T,byvar=Outcome)
#m1_sub_Outcome
forest(m1_sub_Outcome,digits.mean=1,digits.sd=1,subgroup=T)
dev.off()





###########################################################3
#Sub-group analysis ####
# PA only ####
#Q1 - No evidence that PA only leads to much bigger increases
m1_sub_PAOnly_ses <- metacont(data=PArevQ1prim,
                       mean.e=Me,
                       sd.e=Se,
                       n.e=Ne,
                       mean.c=Mc,
                       sd.c=Sc,
                       n.c=Nc,
                       studlab=Author,
                       sm="SMD",
                       hakn=T,byvar=Paonly)
m1_sub_PAOnly_ses

png(file = '/Users/markkelson/Dropbox/SysRev_Apps_PA/Code/Q1_PA_only.png',height=900,width=900) 

forest(m1_sub_PAOnly_ses,digits.mean=1,digits.sd=1,subgroup=T,
       label.right = "Favours intervention",
       label.left = "Favours control")


dev.off() 




# Questions about BC ####

# Low SES- goals and planning versus not ####
m1_sub_Goals_and_planning <- metacont(data=PArevQ1prim,
                       mean.e=Me,
                       sd.e=Se,
                       n.e=Ne,
                       mean.c=Mc,
                       sd.c=Sc,
                       n.c=Nc,
                       studlab=Author,
                       sm="SMD",
                       hakn=T,byvar=Goals_and_planning)

#No evidence of a goal planning effect

#tiff("/Users/markkelson/Dropbox/SysRev_Apps_PA/Code/Q1_BC_Goals_and_planning.tiff", units="in", width=11, height=11, res=300)
png("/Users/markkelson/Dropbox/SysRev_Apps_PA/Code/Q1_BC_Goals_and_planning.png", height=800,width=900)

forest(m1_sub_Goals_and_planning,digits.mean=1,digits.sd=1,subgroup=T,
       label.right = "Favours intervention",
       label.left = "Favours control")

dev.off() 


# Low SES- feedbackandmonitoring versus not ####
m1_sub_Feedback_and_monitoring <- metacont(data=PArevQ1prim,
                          mean.e=Me,
                          sd.e=Se,
                          n.e=Ne,
                          mean.c=Mc,
                          sd.c=Sc,
                          n.c=Nc,
                          studlab=Author,
                          sm="SMD",
                          hakn=T,byvar=Feedback_and_monitoring)

#No evidence of a feedback and monitoring effect

#tiff("/Users/markkelson/Dropbox/SysRev_Apps_PA/Code/Q1_BC_Feedback_and_monitoring.tiff", units="in", width=11, height=11, res=300)
png(file = '/Users/markkelson/Dropbox/SysRev_Apps_PA/Code/Q1_BC_Feedback_and_monitoring.png',height=900,width=900) 

forest(m1_sub_Feedback_and_monitoring,digits.mean=1,digits.sd=1,subgroup=T,
       label.right = "Favours intervention",
       label.left = "Favours control")

dev.off() 


# Low SES- shaping knowledge versus not ####
m1_sub_Sharing_knowledge <- metacont(data=PArevQ1prim,
                          mean.e=Me,
                          sd.e=Se,
                          n.e=Ne,
                          mean.c=Mc,
                          sd.c=Sc,
                          n.c=Nc,
                          studlab=Author,
                          sm="SMD",
                          hakn=T,byvar=Sharing_knowledge)

#No evidence of a shaping knowledge effect

#tiff("/Users/markkelson/Dropbox/SysRev_Apps_PA/Code/Q1_BC_Sharing_knowledge.tiff", units="in", width=11, height=11, res=300)
png(file = '/Users/markkelson/Dropbox/SysRev_Apps_PA/Code/Q1_BC_Sharing_knowledge.png',height=900,width=900) 

forest(m1_sub_Sharing_knowledge,digits.mean=1,digits.sd=1,subgroup=T,
       label.right = "Favours intervention",
       label.left = "Favours control")

dev.off() 

# Low SES- natural consequences versus not ####
m1_sub_Natural_consequences <- metacont(data=PArevQ1prim,
                          mean.e=Me,
                          sd.e=Se,
                          n.e=Ne,
                          mean.c=Mc,
                          sd.c=Sc,
                          n.c=Nc,
                          studlab=Author,
                          sm="SMD",
                          hakn=T,byvar=Natural_consequences)

#No evidence of a natural consequences effect

#tiff("/Users/markkelson/Dropbox/SysRev_Apps_PA/Code/Q1_BC_Natural_consequences.tiff", units="in", width=11, height=11, res=300)
png(file = '/Users/markkelson/Dropbox/SysRev_Apps_PA/Code/Q1_BC_Natural_consequences.png',height=900,width=900) 

forest(m1_sub_Natural_consequences,digits.mean=1,digits.sd=1,subgroup=T,
       label.right = "Favours intervention",
       label.left = "Favours control")

dev.off() 

# Low SES- comparison of behaviour versus not ####
m1_sub_Comparison_of_behaviour <- metacont(data=PArevQ1prim,
                          mean.e=Me,
                          sd.e=Se,
                          n.e=Ne,
                          mean.c=Mc,
                          sd.c=Sc,
                          n.c=Nc,
                          studlab=Author,
                          sm="SMD",
                          hakn=T,byvar=Comparison_of_behaviour)

#No evidence of a comparison of behaviour effect


#tiff("/Users/markkelson/Dropbox/SysRev_Apps_PA/Code/Q1_BC_Comparison_of_behaviour.tiff", units="in", width=11, height=11, res=300)
png(file = '/Users/markkelson/Dropbox/SysRev_Apps_PA/Code/Q1_BC_Comparison_of_behaviour.png',height=900,width=900) 

forest(m1_sub_Comparison_of_behaviour,digits.mean=1,digits.sd=1,subgroup=T,
       label.right = "Favours intervention",
       label.left = "Favours control")

dev.off() 


# Low SES- Reward and threat versus not ####
m1_sub_Reward_and_threat <- metacont(data=PArevQ1prim,
                           mean.e=Me,
                           sd.e=Se,
                           n.e=Ne,
                           mean.c=Mc,
                           sd.c=Sc,
                           n.c=Nc,
                           studlab=Author,
                           sm="SMD",
                           hakn=T,byvar=Reward_and_threat)

#No evidence of a reward and behaviour effect

#tiff("/Users/markkelson/Dropbox/SysRev_Apps_PA/Code/Q1_BC_Reward_and_threat.tiff", units="in", width=11, height=11, res=300)
png(file = '/Users/markkelson/Dropbox/SysRev_Apps_PA/Code/Q1_BC_Reward_and_threat.png',height=900,width=900) 

forest(m1_sub_Reward_and_threat,digits.mean=1,digits.sd=1,subgroup=T,
       label.right = "Favours intervention",
       label.left = "Favours control")

dev.off() 


# Low SES- antecedents versus not ####
m1_sub_Antecedents <- metacont(data=PArevQ1prim,
                           mean.e=Me,
                           sd.e=Se,
                           n.e=Ne,
                           mean.c=Mc,
                           sd.c=Sc,
                           n.c=Nc,
                           studlab=Author,
                           sm="SMD",
                           hakn=T,byvar=Antecedents)

#No evidence of a reward and behaviour effect

#tiff("/Users/markkelson/Dropbox/SysRev_Apps_PA/Code/Q1_BC_Antecedents.tiff", units="in", width=11, height=11, res=300)
png(file = '/Users/markkelson/Dropbox/SysRev_Apps_PA/Code/Q1_BC_Antecedents.png',height=900,width=900) 

forest(m1_sub_Antecedents,digits.mean=1,digits.sd=1,subgroup=T,
       label.right = "Favours intervention",
       label.left = "Favours control")

dev.off() 


# Meta-regression for behaviour change ---------------------------------------------------------

#Q1 number of BC methods app - no evidence of effect ####

m1_sub_TotalBC <- metacont(data=PArevQ1prim,
                             mean.e=Me,
                             sd.e=Se,
                             n.e=Ne,
                             mean.c=Mc,
                             sd.c=Sc,
                             n.c=Nc,
                             studlab=Author,
                             sm="SMD",
                             hakn=T,byvar=Totalforstudy)

metareg_m1_TotalBC <- metareg(m1_sub_TotalBC,~Totalforstudy)

#tiff(file = '/Users/markkelson/Dropbox/SysRev_Apps_PA/Code/Q1_TotalBC.tiff',units="in", width=11, height=11, res=100) 
png(file = '/Users/markkelson/Dropbox/SysRev_Apps_PA/Code/Q1_TotalBC.png',units="in", width=11, height=11, res=100) 

bubble(metareg_m1_TotalBC,main="",
       xlab = "Total number of behaviour change techniques employed",
       col.line = "blue",
       studlab = TRUE,xlim=range(c(-0.2,12)))

dev.off()


#Sub-group analysis ####
#Country- No evidence of country level differences ####
#Q1
m1_sub_Country_ses <- metacont(data=PArevQ1prim,
                              mean.e=Me,
                              sd.e=Se,
                              n.e=Ne,
                              mean.c=Mc,
                              sd.c=Sc,
                              n.c=Nc,
                              studlab=Author,
                              sm="SMD",
                              hakn=T,byvar=Country)
m1_sub_Country_ses

png(file = '/Users/markkelson/Dropbox/SysRev_Apps_PA/Code/Q1_Country.png',height=1000,width=1000) 

forest(m1_sub_Country_ses,digits.mean=1,digits.sd=1,subgroup=T,
       label.right = "Favours intervention",
       label.left = "Favours control")


dev.off() 

#Q3 PAonly
m3_sub_Country_ses <- metacont(data=PArevQ3prim,
                              mean.e=Me,
                              sd.e=Se,
                              n.e=Ne,
                              mean.c=Mc,
                              sd.c=Sc,
                              n.c=Nc,
                              studlab=Author,
                              sm="SMD",
                              hakn=T,byvar=Country)
m3_sub_Country_ses

png(file = '/Users/markkelson/Dropbox/SysRev_Apps_PA/Code/Question3_Country.png',height=900,width=900) 

forest(m3_sub_Country_ses,digits.mean=1,digits.sd=1,subgroup=T,
       label.right = "Favours intervention",
       label.left = "Favours control")


dev.off() 


#Can't do HROB ####
#HROB- No evidence of country level differences ####
m1_sub_HROB <- metacont(data=PArevQ1prim,
                               mean.e=Me,
                               sd.e=Se,
                               n.e=Ne,
                               mean.c=Mc,
                               sd.c=Sc,
                               n.c=Nc,
                               studlab=Author,
                               sm="SMD",
                               hakn=T,byvar=HROB)
m1_sub_HROB

png(file = '/Users/markkelson/Dropbox/SysRev_Apps_PA/Code/Q1_HROB.png',height=1000,width=1000) 

forest(m1_sub_HROB,digits.mean=1,digits.sd=1,subgroup=T,
       label.right = "Favours intervention",
       label.left = "Favours control")


dev.off() 


#Age groups- Disparate categories reported ####
#Chronic disease ####
m1_sub_Chronic_ses <- metacont(data=PArevQ1prim,
                               mean.e=Me,
                               sd.e=Se,
                               n.e=Ne,
                               mean.c=Mc,
                               sd.c=Sc,
                               n.c=Nc,
                               studlab=Author,
                               sm="SMD",
                               hakn=T,byvar=Chronic_disease)
m1_sub_Chronic_ses

png(file = '/Users/markkelson/Dropbox/SysRev_Apps_PA/Code/Q1_Chronic.png',height=900,width=900) 

forest(m1_sub_Chronic_ses,digits.mean=1,digits.sd=1,subgroup=T,
       label.right = "Favours intervention",
       label.left = "Favours control")


dev.off() 

#Duration of app ####
m1_sub_Duration_of_app_m <- metacont(data=PArevQ1prim,
                               mean.e=Me,
                               sd.e=Se,
                               n.e=Ne,
                               mean.c=Mc,
                               sd.c=Sc,
                               n.c=Nc,
                               studlab=Author,
                               sm="SMD",
                               hakn=T,byvar=Duration_of_app_m)
m1_sub_Duration_of_app_m

png(file = '/Users/markkelson/Dropbox/SysRev_Apps_PA/Code/Q1_Duration_of_app_m.png',height=900,width=900) 

forest(m1_sub_Duration_of_app_m,digits.mean=1,digits.sd=1,subgroup=T,
       label.right = "Favours intervention",
       label.left = "Favours control",bysort=T)


dev.off() 



# Meta-regression ---------------------------------------------------------
#Q1 duration of app - no evidence of effect ####
metareg_m1_duration <- metareg(m1_sub_Duration_of_app_m,~Duration_of_app_m)

bubble(metareg_m1_duration,
       xlab = "Duration of app (months)",
       col.line = "blue",
       studlab = TRUE)



#










# Question High SES ####

m3 <- metacont(data=PArevQ3prim,
               mean.e=Me,
               sd.e=Se,
               n.e=Ne,
               mean.c=Mc,
               sd.c=Sc,
               n.c=Nc,
               studlab=Author,
               sm="SMD",
               hakn=T)

forest(m3,digits.mean=1,digits.sd=1)


m3_sub_ses <- metacont(data=PArevQ3prim,
                       mean.e=Me,
                       sd.e=Se,
                       n.e=Ne,
                       mean.c=Mc,
                       sd.c=Sc,
                       n.c=Nc,
                       studlab=Author,
                       sm="SMD",
                       hakn=T,byvar=SES)
m3_sub_ses


#png(file = '/Users/markkelson/Dropbox/SysRev_Apps_PA/Code/Q3.png',height=900,width=900) 
#png(file = '/Users/markkelson/Dropbox/SysRev_Apps_PA/Code/Figure3.png',height=900,width=900) 
pdf(file = '/Users/markkelson/Dropbox/SysRev_Apps_PA/Code/Figure3.pdf',height=10,width=12) 

forest(m3_sub_ses,digits.mean=1,digits.sd=1,subgroup=T,
       label.right = "Favours intervention",
       label.left = "Favours control")

dev.off() 

#Funnel plot
png(file = '/Users/markkelson/Dropbox/SysRev_Apps_PA/Code/Q3_funnel.png',height=900,width=900) 

funnel(m3)

dev.off()

metabias.meta(m3)

round(metabias.meta(m3)[[4]],2)


# Splitting by follow-up time
png(file = '/Users/markkelson/Dropbox/SysRev_Apps_PA/Code/Q3_time.png',height=900,width=900) 

m3_sub_time <- metacont(data=PArevQ3prim,
                        mean.e=Me,
                        sd.e=Se,
                        n.e=Ne,
                        mean.c=Mc,
                        sd.c=Sc,
                        n.c=Nc,
                        studlab=Author,
                        sm="SMD",
                        hakn=T,byvar=Followup_months_cat)
forest(m3_sub_time,digits.mean=1,digits.sd=1,subgroup=T)

dev.off()


# Question in low SES split by outcome

png(file = '/Users/markkelson/Dropbox/SysRev_Apps_PA/Code/Q3_outcome.png',height=1000,width=1000) 

m3_sub_Outcome <- metacont(data=PArevQ3prim,
                           mean.e=Me,
                           sd.e=Se,
                           n.e=Ne,
                           mean.c=Mc,
                           sd.c=Sc,
                           n.c=Nc,
                           studlab=Author,
                           sm="SMD",
                           hakn=T,byvar=Outcome)
#m3_sub_Outcome
forest(m3_sub_Outcome,digits.mean=1,digits.sd=1,subgroup=T)
dev.off()





###########################################################3
#Sub-group analysis ####
# PA only ####
#Q1 - No evidence that PA only leads to much bigger increases
m3_sub_PAOnly_ses <- metacont(data=PArevQ3prim,
                              mean.e=Me,
                              sd.e=Se,
                              n.e=Ne,
                              mean.c=Mc,
                              sd.c=Sc,
                              n.c=Nc,
                              studlab=Author,
                              sm="SMD",
                              hakn=T,byvar=Paonly)
m3_sub_PAOnly_ses

png(file = '/Users/markkelson/Dropbox/SysRev_Apps_PA/Code/Q3_PA_only.png',height=900,width=900) 

forest(m3_sub_PAOnly_ses,digits.mean=1,digits.sd=1,subgroup=T,
       label.right = "Favours intervention",
       label.left = "Favours control")


dev.off() 




# Questions about BC ####

# High SES- goals and planning versus not ####
m3_sub_Goals_and_planning <- metacont(data=PArevQ3prim,
                                      mean.e=Me,
                                      sd.e=Se,
                                      n.e=Ne,
                                      mean.c=Mc,
                                      sd.c=Sc,
                                      n.c=Nc,
                                      studlab=Author,
                                      sm="SMD",
                                      hakn=T,byvar=Goals_and_planning)

#No evidence of a goal planning effect

#tiff("/Users/markkelson/Dropbox/SysRev_Apps_PA/Code/Q3_BC_Goals_and_planning.tiff", units="in", width=11, height=11, res=300)
png("/Users/markkelson/Dropbox/SysRev_Apps_PA/Code/Q3_BC_Goals_and_planning.png", height=800,width=900)

forest(m3_sub_Goals_and_planning,digits.mean=1,digits.sd=1,subgroup=T,
       label.right = "Favours intervention",
       label.left = "Favours control")

dev.off() 


# High SES- feedbackandmonitoring versus not ####
m3_sub_Feedback_and_monitoring <- metacont(data=PArevQ3prim,
                                           mean.e=Me,
                                           sd.e=Se,
                                           n.e=Ne,
                                           mean.c=Mc,
                                           sd.c=Sc,
                                           n.c=Nc,
                                           studlab=Author,
                                           sm="SMD",
                                           hakn=T,byvar=Feedback_and_monitoring)

#No evidence of a feedback and monitoring effect

#tiff("/Users/markkelson/Dropbox/SysRev_Apps_PA/Code/Q3_BC_Feedback_and_monitoring.tiff", units="in", width=11, height=11, res=300)
png(file = '/Users/markkelson/Dropbox/SysRev_Apps_PA/Code/Q3_BC_Feedback_and_monitoring.png',height=900,width=900) 

forest(m3_sub_Feedback_and_monitoring,digits.mean=1,digits.sd=1,subgroup=T,
       label.right = "Favours intervention",
       label.left = "Favours control")

dev.off() 


# High SES- shaping knowledge versus not ####
m3_sub_Sharing_knowledge <- metacont(data=PArevQ3prim,
                                     mean.e=Me,
                                     sd.e=Se,
                                     n.e=Ne,
                                     mean.c=Mc,
                                     sd.c=Sc,
                                     n.c=Nc,
                                     studlab=Author,
                                     sm="SMD",
                                     hakn=T,byvar=Sharing_knowledge)

#No evidence of a shaping knowledge effect

#tiff("/Users/markkelson/Dropbox/SysRev_Apps_PA/Code/Q3_BC_Sharing_knowledge.tiff", units="in", width=11, height=11, res=300)
png(file = '/Users/markkelson/Dropbox/SysRev_Apps_PA/Code/Q3_BC_Sharing_knowledge.png',height=900,width=900) 

forest(m3_sub_Sharing_knowledge,digits.mean=1,digits.sd=1,subgroup=T,
       label.right = "Favours intervention",
       label.left = "Favours control")

dev.off() 

# High SES- natural consequences versus not ####
m3_sub_Natural_consequences <- metacont(data=PArevQ3prim,
                                        mean.e=Me,
                                        sd.e=Se,
                                        n.e=Ne,
                                        mean.c=Mc,
                                        sd.c=Sc,
                                        n.c=Nc,
                                        studlab=Author,
                                        sm="SMD",
                                        hakn=T,byvar=Natural_consequences)

#No evidence of a natural consequences effect

#tiff("/Users/markkelson/Dropbox/SysRev_Apps_PA/Code/Q3_BC_Natural_consequences.tiff", units="in", width=11, height=11, res=300)
png(file = '/Users/markkelson/Dropbox/SysRev_Apps_PA/Code/Q3_BC_Natural_consequences.png',height=900,width=900) 

forest(m3_sub_Natural_consequences,digits.mean=1,digits.sd=1,subgroup=T,
       label.right = "Favours intervention",
       label.left = "Favours control")

dev.off() 

# High SES- comparison of behaviour versus not ####
m3_sub_Comparison_of_behaviour <- metacont(data=PArevQ3prim,
                                           mean.e=Me,
                                           sd.e=Se,
                                           n.e=Ne,
                                           mean.c=Mc,
                                           sd.c=Sc,
                                           n.c=Nc,
                                           studlab=Author,
                                           sm="SMD",
                                           hakn=T,byvar=Comparison_of_behaviour)

#No evidence of a comparison of behaviour effect


#tiff("/Users/markkelson/Dropbox/SysRev_Apps_PA/Code/Q3_BC_Comparison_of_behaviour.tiff", units="in", width=11, height=11, res=300)
png(file = '/Users/markkelson/Dropbox/SysRev_Apps_PA/Code/Q3_BC_Comparison_of_behaviour.png',height=900,width=900) 

forest(m3_sub_Comparison_of_behaviour,digits.mean=1,digits.sd=1,subgroup=T,
       label.right = "Favours intervention",
       label.left = "Favours control")

dev.off() 


# High SES- Reward and threat versus not ####
m3_sub_Reward_and_threat <- metacont(data=PArevQ3prim,
                                     mean.e=Me,
                                     sd.e=Se,
                                     n.e=Ne,
                                     mean.c=Mc,
                                     sd.c=Sc,
                                     n.c=Nc,
                                     studlab=Author,
                                     sm="SMD",
                                     hakn=T,byvar=Reward_and_threat)

#No evidence of a reward and behaviour effect

#tiff("/Users/markkelson/Dropbox/SysRev_Apps_PA/Code/Q3_BC_Reward_and_threat.tiff", units="in", width=11, height=11, res=300)
png(file = '/Users/markkelson/Dropbox/SysRev_Apps_PA/Code/Q3_BC_Reward_and_threat.png',height=900,width=900) 

forest(m3_sub_Reward_and_threat,digits.mean=1,digits.sd=1,subgroup=T,
       label.right = "Favours intervention",
       label.left = "Favours control")

dev.off() 


# High SES- antecedents versus not ####
m3_sub_Antecedents <- metacont(data=PArevQ3prim,
                               mean.e=Me,
                               sd.e=Se,
                               n.e=Ne,
                               mean.c=Mc,
                               sd.c=Sc,
                               n.c=Nc,
                               studlab=Author,
                               sm="SMD",
                               hakn=T,byvar=Antecedents)

#No evidence of a reward and behaviour effect

#tiff("/Users/markkelson/Dropbox/SysRev_Apps_PA/Code/Q3_BC_Antecedents.tiff", units="in", width=11, height=11, res=300)
png(file = '/Users/markkelson/Dropbox/SysRev_Apps_PA/Code/Q3_BC_Antecedents.png',height=900,width=900) 

forest(m3_sub_Antecedents,digits.mean=1,digits.sd=1,subgroup=T,
       label.right = "Favours intervention",
       label.left = "Favours control")

dev.off() 


# Meta-regression for behaviour change ---------------------------------------------------------

#Q1 number of BC methods app - no evidence of effect ####

m3_sub_TotalBC <- metacont(data=PArevQ3prim,
                           mean.e=Me,
                           sd.e=Se,
                           n.e=Ne,
                           mean.c=Mc,
                           sd.c=Sc,
                           n.c=Nc,
                           studlab=Author,
                           sm="SMD",
                           hakn=T,byvar=Totalforstudy)

metareg_m3_TotalBC <- metareg(m3_sub_TotalBC,~Totalforstudy)

tiff(file = '/Users/markkelson/Dropbox/SysRev_Apps_PA/Code/Q3_TotalBC.tiff',units="in", width=11, height=11, res=100) 
png(file = '/Users/markkelson/Dropbox/SysRev_Apps_PA/Code/Q3_TotalBC.png',units="in", width=11, height=11, res=100) 

bubble(metareg_m3_TotalBC,main="",
       xlab = "Total number of behaviour change techniques employed",
       col.line = "blue",
       studlab = TRUE,xlim=range(c(-0.5,12)))


dev.off()


#png(file = '/Users/markkelson/Dropbox/SysRev_Apps_PA/Code/Q1andQ3_TotalBC.png',units="in", width=11, height=11, res=100) 
png(file = '/Users/markkelson/Dropbox/SysRev_Apps_PA/Code/Figure5.png',units="in", width=11, height=11, res=100) 
pdf(file = '/Users/markkelson/Dropbox/SysRev_Apps_PA/Code/Figure5.pdf', width=11, height=11) 

#getting the order of studies    
#metareg_m1_TotalBC$.meta$x$studlab
#metareg_m3_TotalBC$.meta$x$studlab

par(mfrow=c(1,2))
bubble(metareg_m1_TotalBC,main="Low SES",
       sub=paste("Slope = ",
                 round(metareg_m1_TotalBC$beta[2],3),
                 "p-value=",
                 round(metareg_m1_TotalBC$pval[2],2)),
       xlab = "Total number of behaviour change techniques employed",
       col.line = "blue",
       studlab = TRUE,xlim=range(c(-0.5,13)),
       ylim=range(c(-0.8,1.75)),pos.studlab=c(2,2,2,2,4, #1-5
                                              2,2,1,3,1, #6-10
                                              1,2,2,3,1, #11-15
                                              2,1,1,2,1)) #16-20

bubble(metareg_m3_TotalBC,main="High SES",
       sub=paste("Slope = ",
                 round(metareg_m3_TotalBC$beta[2],3),
                 "p-value=",
                 round(metareg_m3_TotalBC$pval[2],2)),
       xlab = "Total number of behaviour change techniques employed",
       col.line = "blue",
       studlab = TRUE,xlim=range(c(-0.5,13)),
       ylim=range(c(-0.8,1.75)),pos.studlab=c(2,4,2,2,2, #1-5
                                              1,3,1,1,2, #6-10
                                              2,2,2,4,2, #11-15
                                              2,2))      #16-17
dev.off()

#Sub-group analysis ####
#Country- No evidence of country level differences ####
#Q1
m3_sub_Country_ses <- metacont(data=PArevQ3prim,
                               mean.e=Me,
                               sd.e=Se,
                               n.e=Ne,
                               mean.c=Mc,
                               sd.c=Sc,
                               n.c=Nc,
                               studlab=Author,
                               sm="SMD",
                               hakn=T,byvar=Country)
m3_sub_Country_ses

png(file = '/Users/markkelson/Dropbox/SysRev_Apps_PA/Code/Q3_Country.png',height=1000,width=1000) 

forest(m3_sub_Country_ses,digits.mean=1,digits.sd=1,subgroup=T,
       label.right = "Favours intervention",
       label.left = "Favours control")


dev.off() 

#Q3 PAonly
m3_sub_Country_ses <- metacont(data=PArevQ3prim,
                               mean.e=Me,
                               sd.e=Se,
                               n.e=Ne,
                               mean.c=Mc,
                               sd.c=Sc,
                               n.c=Nc,
                               studlab=Author,
                               sm="SMD",
                               hakn=T,byvar=Country)
m3_sub_Country_ses

png(file = '/Users/markkelson/Dropbox/SysRev_Apps_PA/Code/Question3_Country.png',height=900,width=900) 

forest(m3_sub_Country_ses,digits.mean=1,digits.sd=1,subgroup=T,
       label.right = "Favours intervention",
       label.left = "Favours control")


dev.off() 


#Can't do HROB ####
#HROB- No evidence of country level differences ####
m3_sub_HROB <- metacont(data=PArevQ3prim,
                        mean.e=Me,
                        sd.e=Se,
                        n.e=Ne,
                        mean.c=Mc,
                        sd.c=Sc,
                        n.c=Nc,
                        studlab=Author,
                        sm="SMD",
                        hakn=T,byvar=HROB)
m3_sub_HROB

png(file = '/Users/markkelson/Dropbox/SysRev_Apps_PA/Code/Q3_HROB.png',height=1000,width=1000) 

forest(m3_sub_HROB,digits.mean=1,digits.sd=1,subgroup=T,
       label.right = "Favours intervention",
       label.left = "Favours control")


dev.off() 


#Age groups- Disparate categories reported ####
#Chronic disease ####
m3_sub_Chronic_ses <- metacont(data=PArevQ3prim,
                               mean.e=Me,
                               sd.e=Se,
                               n.e=Ne,
                               mean.c=Mc,
                               sd.c=Sc,
                               n.c=Nc,
                               studlab=Author,
                               sm="SMD",
                               hakn=T,byvar=Chronic_disease)
m3_sub_Chronic_ses

png(file = '/Users/markkelson/Dropbox/SysRev_Apps_PA/Code/Q3_Chronic.png',height=900,width=900) 

forest(m3_sub_Chronic_ses,digits.mean=1,digits.sd=1,subgroup=T,
       label.right = "Favours intervention",
       label.left = "Favours control")


dev.off() 

#Duration of app ####
m3_sub_Duration_of_app_m <- metacont(data=PArevQ3prim,
                                     mean.e=Me,
                                     sd.e=Se,
                                     n.e=Ne,
                                     mean.c=Mc,
                                     sd.c=Sc,
                                     n.c=Nc,
                                     studlab=Author,
                                     sm="SMD",
                                     hakn=T,byvar=Duration_of_app_m)
m3_sub_Duration_of_app_m

png(file = '/Users/markkelson/Dropbox/SysRev_Apps_PA/Code/Q3_Duration_of_app_m.png',height=900,width=900) 

forest(m3_sub_Duration_of_app_m,digits.mean=1,digits.sd=1,subgroup=T,
       label.right = "Favours intervention",
       label.left = "Favours control",bysort=T)


dev.off() 



# Meta-regression ---------------------------------------------------------
#Q1 duration of app - no evidence of effect ####
metareg_m3_duration <- metareg(m3_sub_Duration_of_app_m,~Duration_of_app_m)

bubble(metareg_m3_duration,
       xlab = "Duration of app (months)",
       col.line = "blue",
       studlab = TRUE)



#







# Legacy question 3 stuff ####
# Question in high SES#####

m3 <- metacont(data=PArevQ3prim,
               mean.e=Me,
               sd.e=Se,
               n.e=Ne,
               mean.c=Mc,
               sd.c=Sc,
               n.c=Nc,
               studlab=Author,
               sm="SMD",
               hakn=T)

forest(m3,digits.mean=1,digits.sd=1)


m3_sub_ses <- metacont(data=PArevQ3prim,
                       mean.e=Me,
                       sd.e=Se,
                       n.e=Ne,
                       mean.c=Mc,
                       sd.c=Sc,
                       n.c=Nc,
                       studlab=Author,
                       sm="SMD",
                       hakn=T, byvar=SES)

#png(file = '/Users/markkelson/Dropbox/SysRev_Apps_PA/Code/Question3-high.png',height=900,width=900) 
tiff(file = '/Users/markkelson/Dropbox/SysRev_Apps_PA/Code/Question3-high.tiff',units="in", width=11, height=11, res=200) 

forest(m3_sub_ses,digits.mean=1,digits.sd=1,subgroup=T,
       label.right = "Favours intervention",
       label.left = "Favours control")

dev.off() 


#Funnel plot

funnel(m3)

metabias()


#Q3 PAonly - doesnot change results
m3_sub_PAOnly_ses <- metacont(data=PArevQ3prim,
                              mean.e=Me,
                              sd.e=Se,
                              n.e=Ne,
                              mean.c=Mc,
                              sd.c=Sc,
                              n.c=Nc,
                              studlab=Author,
                              sm="SMD",
                              hakn=T,byvar=Paonly)
m3_sub_PAOnly_ses

png(file = '/Users/markkelson/Dropbox/SysRev_Apps_PA/Code/Q3_PA_only.png',height=900,width=900) 

forest(m3_sub_PAOnly_ses,digits.mean=1,digits.sd=1,subgroup=T,
       label.right = "Favours intervention",
       label.left = "Favours control")


dev.off() 


#Q3 total number of behaviour change methods - no evidence of effect ####


m3_sub_TotalBC <- metacont(data=PArevQ3prim,
                           mean.e=Me,
                           sd.e=Se,
                           n.e=Ne,
                           mean.c=Mc,
                           sd.c=Sc,
                           n.c=Nc,
                           studlab=Author,
                           sm="SMD",
                           hakn=T,byvar=Totalforstudy)

metareg_m3_TotalBC <- metareg(m3_sub_TotalBC,~Totalforstudy)

tiff(file = '/Users/markkelson/Dropbox/SysRev_Apps_PA/Code/Question3_TotalBC.tiff',units="in", width=11, height=11, res=100) 
png(file = '/Users/markkelson/Dropbox/SysRev_Apps_PA/Code/Q3_TotalBC.tiff',units="in", width=11, height=11, res=100) 

bubble(metareg_m3_TotalBC,main="",
       xlab = "Total number of behaviour change techniques employed",
       col.line = "blue",
       studlab = TRUE)

dev.off()


#Q3 HROB
m3_sub_HROB <- metacont(data=PArevQ3prim,
                        mean.e=Me,
                        sd.e=Se,
                        n.e=Ne,
                        mean.c=Mc,
                        sd.c=Sc,
                        n.c=Nc,
                        studlab=Author,
                        sm="SMD",
                        hakn=T,byvar=HROB)
m3_sub_HROB

png(file = '/Users/markkelson/Dropbox/SysRev_Apps_PA/Code/Q3_HROB.png',height=900,width=900) 

forest(m3_sub_HROB,digits.mean=1,digits.sd=1,subgroup=T,
       label.right = "Favours intervention",
       label.left = "Favours control")


dev.off() 

#Q3 Chronic
m3_sub_Chronic_ses <- metacont(data=PArevQ3prim,
                               mean.e=Me,
                               sd.e=Se,
                               n.e=Ne,
                               mean.c=Mc,
                               sd.c=Sc,
                               n.c=Nc,
                               studlab=Author,
                               sm="SMD",
                               hakn=T,byvar=Chronic_disease)
m3_sub_Chronic_ses

png(file = '/Users/markkelson/Dropbox/SysRev_Apps_PA/Code/Question3_Chronic.png',height=900,width=900) 

forest(m3_sub_Chronic_ses,digits.mean=1,digits.sd=1,subgroup=T,
       label.right = "Favours intervention",
       label.left = "Favours control")


dev.off() 


#Q3 Duration of app ####
m3_sub_Duration_of_app_m <- metacont(data=PArevQ3prim,
                                     mean.e=Me,
                                     sd.e=Se,
                                     n.e=Ne,
                                     mean.c=Mc,
                                     sd.c=Sc,
                                     n.c=Nc,
                                     studlab=Author,
                                     sm="SMD",
                                     hakn=T,byvar=Duration_of_app_m)
m3_sub_Duration_of_app_m

png(file = '/Users/markkelson/Dropbox/SysRev_Apps_PA/Code/Question3_Duration_of_app_m.png',height=900,width=900) 

forest(m3_sub_Duration_of_app_m,digits.mean=1,digits.sd=1,subgroup=T,
       label.right = "Favours intervention",
       label.left = "Favours control",bysort=T)


dev.off() 

###Q3 duration of app - no evidence of effect ####
metareg_m3_duration <- metareg(m3_sub_Duration_of_app_m,~Duration_of_app_m)


bubble(metareg_m3_duration,
       xlab = "Duration of app (months)",
       col.line = "blue",
       studlab = TRUE)



#################################################
#################################################
#################################################
#################################################
#################################################
#################################################
#################################################
#IJBNPA revisions
#Self-report v Objective report

library(grid)
#Q1 - 
m1_sub_SRvOR_ses <- metacont(data=PArevQ1prim,
                              mean.e=Me,
                              sd.e=Se,
                              n.e=Ne,
                              mean.c=Mc,
                              sd.c=Sc,
                              n.c=Nc,
                              studlab=Author,
                              sm="SMD",
                              hakn=T,byvar=SelfReportPA)
m1_sub_SRvOR_ses

png(file = '/Users/markkelson/Dropbox/SysRev_Apps_PA/Code/Q1_sub_SRvOR_ses.png',height=900,width=900) 

forest(m1_sub_SRvOR_ses,digits.mean=1,digits.sd=1,subgroup=T,
       label.right = "Favours intervention",
       label.left = "Favours control")

grid.text("Low SES", .13, .85, gp=gpar(cex=2))


dev.off() 

#Q3 - 
m3_sub_SRvOR_ses <- metacont(data=PArevQ3prim,
                             mean.e=Me,
                             sd.e=Se,
                             n.e=Ne,
                             mean.c=Mc,
                             sd.c=Sc,
                             n.c=Nc,
                             studlab=Author,
                             sm="SMD",
                             hakn=T,byvar=SelfReportPA)
m3_sub_SRvOR_ses

png(file = '/Users/markkelson/Dropbox/SysRev_Apps_PA/Code/Q3_sub_SRvOR_ses.png',height=900,width=900) 

forest(m3_sub_SRvOR_ses,digits.mean=1,digits.sd=1,subgroup=T,
       label.right = "Favours intervention",
       label.left = "Favours control")

grid.text("High SES", .14, .8, gp=gpar(cex=2))

dev.off() 

###########################################
#Active control versus inactive control

#Q1 - 
m1_sub_ACvIC_ses <- metacont(data=PArevQ1prim,
                             mean.e=Me,
                             sd.e=Se,
                             n.e=Ne,
                             mean.c=Mc,
                             sd.c=Sc,
                             n.c=Nc,
                             studlab=Author,
                             sm="SMD",
                             hakn=T,byvar=Active_Control)
m1_sub_ACvIC_ses

png(file = '/Users/markkelson/Dropbox/SysRev_Apps_PA/Code/Q1_sub_ACvIC_ses.png',height=900,width=900) 

forest(m1_sub_ACvIC_ses,digits.mean=1,digits.sd=1,subgroup=T,
       label.right = "Favours intervention",
       label.left = "Favours control")

grid.text("Low SES", .13, .85, gp=gpar(cex=2))


dev.off() 

#Q3 - 
m3_sub_ACvIC_ses <- metacont(data=PArevQ3prim,
                             mean.e=Me,
                             sd.e=Se,
                             n.e=Ne,
                             mean.c=Mc,
                             sd.c=Sc,
                             n.c=Nc,
                             studlab=Author,
                             sm="SMD",
                             hakn=T,byvar=Active_Control)
m3_sub_ACvIC_ses

png(file = '/Users/markkelson/Dropbox/SysRev_Apps_PA/Code/Q3_sub_ACvIC_ses.png',height=900,width=900) 

forest(m3_sub_ACvIC_ses,digits.mean=1,digits.sd=1,subgroup=T,
       label.right = "Favours intervention",
       label.left = "Favours control")

grid.text("High SES", .14, .8, gp=gpar(cex=2))

dev.off() 





