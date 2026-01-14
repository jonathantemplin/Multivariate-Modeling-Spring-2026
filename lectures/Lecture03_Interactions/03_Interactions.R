
#LOAD DATA
data01 = read.csv("03_InteractionsData.csv")
#-----------------------------------------------------------------------------------------------------------
#AUTOMATING PACKAGES NEEDED FOR ANALYSES
haspackage = require("multcomp")
if (haspackage==FALSE){
  install.packages("multcomp")
}
library("multcomp")
#-----------------------------------------------------------------------------------------------------------
#Create FACTOR variables for senior, new teaching method, and group
data01$GroupF = factor(data01$Group)
data01$NewF = factor(data01$New)
data01$SeniorF = factor(data01$Senior)

#Slide 3: Summary statistics by group (joint distribution of senior*new):
mean_group=by(data01$Test,data01$GroupF,mean)
sd_group=by(data01$Test,data01$GroupF,sd)
stderr_group=by(data01$Test,data01$GroupF,function(x) sd(x)/sqrt(length(x)))
groupstats = cbind(mean_group,sd_group,stderr_group)
show(groupstats)

#Slide 3: Summary statistics by Senior (Marginal distribution of senior):
mean_senior=by(data01$Test,data01$SeniorF,mean)
sd_senior=by(data01$Test,data01$SeniorF,sd)
stderr_senior=by(data01$Test,data01$SeniorF,function(x) sd(x)/sqrt(length(x)))
seniorstats = cbind(mean_senior,sd_senior,stderr_senior)
show(seniorstats)

#Slide 3: Summary statistics by New (Marginal distribution of New):
mean_new=by(data01$Test,data01$NewF,mean)
sd_new=by(data01$Test,data01$NewF,sd)
stderr_new=by(data01$Test,data01$NewF,function(x) sd(x)/sqrt(length(x)))
newstats = cbind(mean_new,sd_new,stderr_new)
show(newstats)

#Slide 3: Overall Statisics:
mean_overall=mean(data01$Test)
sd_overall=sd(data01$Test)
stderr_overall=sd(data01$Test)/sqrt(length(data01$Test))
overallstats = cbind(mean_overall,sd_overall,stderr_overall)
show(overallstats)

#MODEL #1 -- Using 0/1 coding instead of factors
model1 = lm(Test~Senior+New+Senior*New,data=data01)
summary(model1)
anova(model1)

mean1 = matrix(c(1,0,0,0),1); rownames(mean1)="Freshman-Old"
mean2 = matrix(c(1,0,1,0),1); rownames(mean2)="Freshman-New"
mean3 = matrix(c(1,1,0,0),1); rownames(mean3)="Senior-Old"
mean4 = matrix(c(1,1,1,1),1); rownames(mean4)="Senior-New"

meansvec = rbind(mean1,mean2,mean3,mean4)
means = glht(model1,linfct=meansvec)
summary(means)

effect1 = matrix(c(0,1,0,0),1); rownames(effect1) = "Senior Effect: Old"
effect2 = matrix(c(0,1,0,1),1); rownames(effect2) = "Senior Effect: New"
effect3 = matrix(c(0,0,1,0),1); rownames(effect3) = "New Effect: Freshmen"
effect4 = matrix(c(0,0,1,1),1); rownames(effect4) = "New Effect: Seniors"

effectsvec = rbind(effect1,effect2,effect3,effect4)
effects = glht(model1,linfct=effectsvec)
summary(effects)

#MODEL #2 -- Using factors (R coded)
model2 = lm(Test~SeniorF+NewF+SeniorF*NewF,data=data01)
summary(model2)
anova(model2)

mean1 = matrix(c(1,0,0,0),1); rownames(mean1)="Freshman-Old"
mean2 = matrix(c(1,0,1,0),1); rownames(mean2)="Freshman-New"
mean3 = matrix(c(1,1,0,0),1); rownames(mean3)="Senior-Old"
mean4 = matrix(c(1,1,1,1),1); rownames(mean4)="Senior-New"

meansvec = rbind(mean1,mean2,mean3,mean4)
means = glht(model2,linfct=meansvec)
summary(means)

effect1 = matrix(c(0,1,0,0),1); rownames(effect1) = "Senior Effect: Old"
effect2 = matrix(c(0,1,0,1),1); rownames(effect2) = "Senior Effect: New"
effect3 = matrix(c(0,0,1,0),1); rownames(effect3) = "New Effect: Freshmen"
effect4 = matrix(c(0,0,1,1),1); rownames(effect4) = "New Effect: Seniors"

effectsvec = rbind(effect1,effect2,effect3,effect4)
effects = glht(model2,linfct=effectsvec)
summary(effects)

