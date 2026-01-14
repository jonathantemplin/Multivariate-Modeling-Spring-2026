
#LOAD EXTERNAL R DATA SET
load("dataSexHeightWeight.rda")

#ASSIGN A DIFFERENT NAME FOR THE DATA SET TO MAKE CHANGES
data01 = dataSexHeightWeight

#CREATE NEW VARIABLE WHERE FEMALE=1 (DUMMY CODED)
data01$female[data01$sex=="F"]=1
data01$female[data01$sex=="M"]=0

#SCATTER PLOT MATRIX:
pairs(~female+heightIN+weightLB,data=data01)

#HISTOGRAM OF DATA FOR EACH QUANTITATIVE VARIABLE:
par(mfrow=c(1,2)) #SET PLOT TO SHOW TWO PICTURES IN ONE ROW
hist(data01$weightLB,main="Weight",xlab="Pounds")
hist(data01$heightIN,main="Height",xlab="Inches")
par(mfrow=c(1,1)) #SET PLOT TO SHOW ONE PICTURE

#DESCRIPTIVE STATISTICS
summary(data01$heightIN)
summary(data01$weightLB)
summary(data01$female)

#COVARIANCE MATRIX (USING N-1)
var(cbind(data01$heightIN,data01$weightLB,data01$female))

#CORRELATION MATRIX (USING N-1 IN THE COVARIANCE)
correlationmatix = cor(cbind(data01$heightIN,data01$weightLB,data01$female))
correlationmatix

#ASSIGN A DIFFERENT NAME FOR THE DATA SET TO MAKE CHANGES
data01 = dataSexHeightWeight

#CREATE NEW VARIABLE WHERE FEMALE=1 (DUMMY CODED)
data01$female[data01$sex=="F"]=1
data01$female[data01$sex=="M"]=0

#Model #1: Empty model
model01 = lm(weightLB~1,data=data01)
summary(model01)
anova(model01)

#Model #2: Adding Height
model02 = lm(weightLB~heightIN,data=data01)
summary(model02)
anova(model02)

#model comparison with model #1:
anova(model01,model02)

#center height at mean height
data01$heightIN_MC = data01$heightIN - mean(data01$heightIN)

#model #2a: Adding mean-centered height
model02a = lm(weightLB~heightIN_MC,data=data01)
summary(model02a)
anova(model02a)

#model comparison with model #1:
anova(model01,model02a)

#model comparison with model #2:
anova(model02,model02a)


#model #3: adding female to empty model:
model03 = lm(weightLB~female,data=data01)

summary(model03)
anova(model03)

#model comparison with model #1:
anova(model01,model03)

#model #4: adding both MCheight and female to empty model
model04 = lm(weightLB~heightIN_MC+female,data=data01)

summary(model04)
anova(model04)

#model comparison with model #2:
anova(model02a,model04)

#model comparison with model #3:
anova(model03,model04)

#model comparison with model #1:
anova(model01,model04)

#model #5: adding interaction to model #4:
model05 = lm(weightLB~heightIN_MC+female+female*heightIN_MC,data=data01)

summary(model05)
anova(model05)

anova(model04,model05)

#PLOTTING REGRESSION LINES FOR MODEL 5
plot(data01$heightIN,data01$weightLB,col="blue",pch=16,xlab="Height (inches)",ylab="Weight (pounds)")
#add regression line
