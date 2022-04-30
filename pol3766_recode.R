
##import dataset to R
setwd("C:/Users/Caity/Downloads")
fulldat<-read.csv("POL3766_DATA.CSV")
##create a new dataframe that only inlcudes individuals that finished, are 18, and consented which leaves us with 34 observations
dat<-subset(fulldat, Finished==1&fulldat$X18yo==1&consentq==1&DistributionChannel=="anonymous")
##first have to make general column that scores each participant 
##remove NA values 
dat[is.na(dat)]=0
##sum respondent scores
for(i in 1:nrow(dat)){
  dat$respondentscore[i]<-sum(dat$Pew.respondent.1[i], dat$Pew.respondent.2[i], dat$Pew.respondent.3[i], dat$Pew.respondent.4[i], dat$Pew.respondent.5[i], dat$Pew.respondent.6[i], dat$Pew.respondent.7[i], dat$Pew.respondent.8[i], dat$Pew.respondent.9[i], dat$Pew.respondent.10[i])
}

##assign value
##In the pew respondent, I did the scale a little differently. 
for(x in 1:nrow(dat)){
  if(dat$respondentscore[x]>=-10){
    dat$respondentideo[x]<-"Strongly Liberal"
  }
  if(dat$respondentscore[x]>=-7){
    dat$respondentideo[x]<-"Moderately Liberal"
  }
  if(dat$respondentscore[x]>=-4){
    dat$respondentideo[x]<-"Slightly Liberal"
  }
  if(dat$respondentscore[x]>=-1){
    dat$respondentideo[x]<-"Moderate"
  }
  if(dat$respondentscore[x]>=2){
    dat$respondentideo[x]<-"Slightly Conservative"
  }
  if(dat$respondentscore[x]>=4){
    dat$respondentideo[x]<-"Moderately Conservative"
  }
  if(dat$respondentscore[x]>=7){
    dat$respondentideo[x]<-"Strongly Conservative"
  }}
##Now, we have to assign calculations for the parent unit 
for(y in 1:nrow(dat)){
  (dat$parentscore[y]<-sum(dat$pew1[y],dat$pew2[y],dat$pew3[y],dat$pew4[y],dat$pew5[y],dat$pew6[y],dat$pew7[y],dat$pew8[y],dat$pew9[y],dat$pew10[y]))
}
dat$parentscore    
for(z in 1:nrow(dat)){
  if(dat$parentscore[z]>=-10){
    dat$parentideo[z]<-"Strongly Liberal"
  }
  if(dat$parentscore[z]>=-7){
    dat$parentideo[z]<-"Moderately Liberal"
  }
  if(dat$parentscore[z]>=-4){
    dat$parentideo[z]<-"Slightly Liberal"
  }
  if(dat$parentscore[z]>=-1){
    dat$parentideo[z]<-"Moderate"
  }
  if(dat$parentscore[z]>=2){
    dat$parentideo[z]<-"Slightly Conservative"
  }
  if(dat$parentscore[z]>=4){
    dat$parentideo[z]<-"Moderately Conservative"
  }
  if(dat$parentscore[z]>=7){
    dat$parentideo[z]<-"Strongly Conservative"
  }}
dat$parentideo  
##now to assign values for people who did not feel comfortable placing individual points
for(a in 1:nrow(dat)){
  if(dat$ideologyp[a]==7){
    dat$parentideo[a]<-"Strongly Liberal"
  }
  if(dat$ideologyp[a]==6){
    dat$parentideo[a]<-"Moderately Liberal"
  }
  if(dat$ideologyp[a]==5){
    dat$parentideo[a]<-"Slightly Liberal"
  }
  if(dat$ideologyp[a]==4){
    dat$parentideo[a]<-"Moderate"
  }
  if(dat$ideologyp[a]==3){
    dat$parentideo[a]<-"Slightly Liberal"
  }
  if(dat$ideologyp[a]==2){
    dat$parentideo[a]<-"Moderately Liberal"
  }
  if(dat$ideologyp[a]==1){
    dat$parentideo[a]<-"Strongly Liberal"
  }}
colnames(dat)
for(b in 1:nrow(dat)){
  if(dat$ideologyp[b]==1){
    dat$parentscore[b]<-1
  }
  if(dat$ideologyp[b]==5){
    dat$parentscore[b]<-5
  }}
dat$parentscore
##alright, now every participant has a parent score (parentscore), parent ideology (parentideo)
##and a respondent score (respondentscore) and ideology (respondentideo).

##now, to test our first hypothesis we want to see whether individuals match their parents on thse scores.
##unfortunately, due to our data a linear relationship is inappropriate test.
plot(dat$respondentscore, dat$parentscore)
mod<-lm(dat$parentscore~dat$respondentscore)
anova(mod)
mod
abline(mod)
table(dat$respondentideo==dat$parentideo)
14/34
for(c in 1:nrow(dat)){
  if(dat$parentscore[c]>=-10){
    dat$parentgenideo[c]<-"Liberal"
  }
  if(dat$parentscore[c]>=-1){
    dat$parentgenideo[c]<-"Moderate"
  }
  if(dat$parentscore[c]>=2){
    dat$parentgenideo[c]<-"Conservative"
  }
}
dat$parentgenideo
dat$parentideo
for(d in 1:nrow(dat)){
  if(dat$respondentscore[d]>=-10){
    dat$respondentgenideo[d]<-"Liberal"
  }
  if(dat$respondentscore[d]>=-1){
    dat$respondentgenideo[d]<-"Moderate"
  }
  if(dat$respondentscore[d]>=2){
    dat$respondentgenideo[d]<-"Conservative"
  }
}
dat$respondentgenideo
table(dat$respondentgenideo==dat$parentgenideo)
24/34
summary(dat$respondentscore)
summary(dat$parentscore)
table(dat$forma1)
table(dat$forma2)
table(dat$forma3)
table(dat$pcomf)
##now that we have some general statistics out of the way, it's time for the fun part. the parent socialization.
#* = permissive, 2* = authoritarian, 3 = authoritative
#* permissive items 1-7, authoritarian = 8-14, authoritative = 15-20

for(f in 1:nrow(dat)){
  dat$parentpermscore[f]<-sum(dat$paqmatrix_1[f],dat$paqmatrix_2[f],dat$paqmatrix_3[f],dat$paqmatrix_4[f],dat$paqmatrix_5[f],dat$paqmatrix_6[f],dat$paqmatrix_7[f])
  dat$parentauthnscore[f]<-sum(dat$paqmatrix_8[f],dat$paqmatrix_9[f],dat$paqmatrix_10[f],dat$paqmatrix_11[f],dat$paqmatrix_12[f],dat$paqmatrix_13[f],dat$paqmatrix_14[f])
  dat$parentauthvscore[f]<-sum(dat$paqmatrix_15[f],dat$paqmatrix_16[f],dat$paqmatrix_17[f],dat$paqmatrix_18[f],dat$paqmatrix_19[f],dat$paqmatrix_20[f])
}
for(f in 1:nrow(dat)){
  dat$parentpermavg[f]<-mean(dat$paqmatrix_1[f],dat$paqmatrix_2[f],dat$paqmatrix_3[f],dat$paqmatrix_4[f],dat$paqmatrix_5[f],dat$paqmatrix_6[f],dat$paqmatrix_7[f])
  dat$parentauthnavg[f]<-mean(dat$paqmatrix_8[f],dat$paqmatrix_9[f],dat$paqmatrix_10[f],dat$paqmatrix_11[f],dat$paqmatrix_12[f],dat$paqmatrix_13[f],dat$paqmatrix_14[f])
  dat$parentauthvavg[f]<-mean(dat$paqmatrix_15[f],dat$paqmatrix_16[f],dat$paqmatrix_17[f],dat$paqmatrix_18[f],dat$paqmatrix_19[f],dat$paqmatrix_20[f])
}
dat$parentpermavg
dat$parentauthnavg
dat$parentauthvavg
colnames(dat)[max.col(dat, ties.method="last")]
library(dplyr)
library(data.table)
dat2<-as.data.frame(dat2 %>% mutate(class=names(.)[max.col(.)]))
dat2<-data.table(dat$parentpermscore, dat$parentauthnscore, dat$parentauthvscore)
library(dplyr)
dat2<-as.data.frame(dat2 %>% mutate(class=names(.)[max.col(.)]))
##this one ^
colnames(dat2)<-c("1", "2", "3")
dat2<-data.table(dat$parentpermscore, dat$parentauthnscore, dat$parentauthvscore, dat$respondentscore, dat$parentscore, dat2$class)
colnames(dat2)<-c("parentpermscore", "parentauthnscore", "parentauthvscore", "respondentscore", "parentscore", "class")
mod<-lm(dat2$respondentscore~dat2$parentscore*dat2$class)
anova(mod)
summary(mod)
perm<-subset(dat2, dat2$class==1)
authv<-subset(dat2, dat2$class==2)
authn<-subset(dat2, dat2$class==3)
t.test(perm$respondentscore, perm$parentscore)
t.test(authv$respondentscore, authv$parentscore)
t.test(authn$respondentscore, authn$parentscore)
library(ggplot2)
library(tidyr)
ggplot(dat2, aes(class,parentscore))+geom_boxplot()
dat2_long<-pivot_longer(dat2, cols=c("parentscore", "respondentscore"), names_to="por", values_to="scores")
ggplot(dat2_long, aes(class,scores,fill=por))+geom_boxplot()+theme_minimal()+xlab(label="Parenting Style")+ylab(label="Ideology Score")+labs(fill="")+scale_fill_discrete(labels=c("Parent Score", "Respondent Score"))
ggsave("parentstyle_score.png")
dat[30,]
dat2<-dat2[-30,]
##graph specific stuff.
##save dat 2 as own file to avoid this randomness stuff more !!!!!
write.csv(dat2,"pol_recode_dat2.csv")
##caregiver
ggplot(dat, aes(forma1))+geom_bar(fill="lightblue")+theme_minimal()+xlab(label="Primary Caregiver ID")+ylab(label="Frequency")
dat$forma1<-factor(dat$forma1, levels=c("Mother", "Father", "Grandparent", "Other"))
levels(dat$forma1)=levels=c("Mother", "Father", "Grandparent", "Other")
ggsave("caregiver.png", dpi=300)
ggplot(dat, aes(forma2))+geom_bar(fill="lightblue")+theme_minimal()+xlab(label="Perceived Discussion of Politics")+ylab(label="Frequency")
dat$forma2<-as.factor(dat$forma2)
levels(dat$forma2)<-(levels=c("A lot of the time", "Sometimes", "Rarely", "Never"))
ggsave("discussion.png", dpi=300)
ggplot(dat, aes(forma3))+geom_bar(fill="lightblue")+theme_minimal()+xlab(label="Parental Education")+ylab(label="Frequency")
dat$forma3<-as.factor(dat$forma3)
levels(dat$forma3)<-(levels=c("No high school degree","High school","Some college","2yr college","4yr college","Professional degree"))
ggsave("parental_ed.png", dpi=600)
colnames(dat)

##ideology
ggplot(dat2_long, aes(scores,fill=por))+geom_bar()+theme_minimal()+xlab(label="Ideology Score")+ylab(label="Frequency")+labs(fill="")+scale_fill_discrete(labels=c("Parent Score", "Respondent Score"))
ggsave("ideology.png")

cor.test(dat$respondentscore,dat$parentscore)
ggplot(dat, aes(respondentscore, parentscore))+geom_point()+xlab(label="Respondent Score")+ylab(label="Parent Score")
