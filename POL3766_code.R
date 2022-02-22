
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
for(g in 1:nrow(dat)){
  dat$parentpermavg[g]<-mean(dat$parentpermscore[g])
  dat$parentauthnavg[g]<-mean(dat$parentauthnscore[g])
  dat$parentauthvavg[g]<-mean(dat$parentauthvscore[g])
}
dat$parentpermavg
dat$parentauthnavg
dat$parentauthvavg
dat$parentpermavg==dat$parentauthvavg|dat$parentpermavg==dat$parentauthnavg|dat$parentauthnavg==dat$parentauthvavg
max(c(dat$parentpermscore, dat$parentauthnscore))
max(dat$parentauthnavg)
library(data.table)
dat2<-data.table(dat$parentpermavg, dat$parentauthnavg, dat$parentauthvavg)
dat2[, max:=pmax(dat$parentpermavg, dat$parentauthnavg, dat$parentauthvavg)]
colnames(dat2)[max.col(dat, ties.method="random")]
library(dplyr)
dat2<-as.data.frame(dat2 %>% mutate(class=names(.)[max.col(.)]))
colnames(dat2)<-c("1", "2", "3")
dat$parentingstyle<-dat2$class
dat$parentingstyle
write.csv(dat, "pol3766_datacoded.csv")
dat3<-read.csv("pol3766_datacoded.csv")
dat3$parentingstyle
dat3$parentpermavg
dat3$parentauthnavg
dat3$parentauthvavg
mod<-lm(dat3$respondentscore~dat3$parentingstyle*dat3$parentscore)
anova(mod)
boxplot(dat3$parentscore~dat3$parentingstyle)
boxplot(dat3$respondentscore~dat3$parentingstyle)
mod2<-lm(dat3$respondentscore~dat3$parentingstyle)
anova(mod2)
library(ggplot2)
ggplot(dat3, aes(dat3$respondentscore, dat3$parentingscore, color=dat3$parentingstyle))+geom_line()
