if (!require(psych)){
  install.packages("psych")
}
library(psych)

if (!require(ordinal)){
  install.packages("ordinal")  
}
library(ordinal)


data01 = read.csv("ologit.csv")

# creating new variables:

# Lapply: Binary variable to show two-category case
data01$Lapply = 0
data01$Lapply[which(data01$APPLY == 0)] = 0
data01$Lapply[which(data01$APPLY == 1 | data01$APPLY == 2)] = 1

# centering GPA at 3:
data01$GPA3 = data01$GPA - 3

# showing means of all variables
describe(data01)

# showing table of all categorical variables:
table(data01$Lapply)

table(data01$APPLY)

table(data01$PARED)

table(data01$PUBLIC)

# response variable must be a factor:
data01$LapplyF = factor(data01$Lapply)


# EMPTY MODEL PREDICTING DICHOTOMOUS (0/1): 
model01 = clm(formula = LapplyF ~ 1, data = data01, control = clm.control(trace = 1))
summary(model01)

# probability of likely to apply
exp(-1*model01$coefficients[1])/(1+exp(-1*model01$coefficients[1]))
mean(data01$Lapply)

# MODEL 02: ADDING PREDICTORS TO THE EMPTY MODEL
model02 = clm(formula = LapplyF ~ 1 + PARED + PUBLIC + GPA3, 
              data = data01, control = clm.control(trace = 1))

anova(model01, model02)
summary(model02)

# odds of likely to apply w/parental graduate degree:
exp(-1*model02$coefficients[1]+model02$coefficients[2])

# odds of likely to apply w/0 parental graduate degree:
exp(-1*model02$coefficients[1])

# odds ratio:
exp(-1*model02$coefficients[1]+model02$coefficients[2]) / exp(-1*model02$coefficients[1])

# equal to slope:
exp(model02$coefficients[2])

# for public
-1*model02$coefficients[1]+model02$coefficients[3]
-1*model02$coefficients[1]+model02$coefficients[3]*0

exp(-1*model02$coefficients[1]+model02$coefficients[3])
exp(-1*model02$coefficients[1]+model02$coefficients[3]*0)

exp(-1*model02$coefficients[1]+model02$coefficients[3])/(1+exp(-1*model02$coefficients[1]+model02$coefficients[3]))
exp(-1*model02$coefficients[1]+model02$coefficients[3]*0)/(1+exp(-1*model02$coefficients[1]+model02$coefficients[3]*0))


# Probability of likely to apply w/parental graduate degree:
exp(-1*model02$coefficients[1]+model02$coefficients[2])/ (1+exp(-1*model02$coefficients[1]+model02$coefficients[2]))

# odds of likely to apply w/0 parental graduate degree:
exp(-1*model02$coefficients[1])/(1+exp(-1*model02$coefficients[1]))

# looking at slopes of GPA3:
# logit:
-1*model02$coefficients[1]+model02$coefficients[4]*1
-1*model02$coefficients[1]+model02$coefficients[4]*0
-1*model02$coefficients[1]+model02$coefficients[4]*-1
-1*model02$coefficients[1]+model02$coefficients[4]*-2

# odds:
exp(-1*model02$coefficients[1]+model02$coefficients[4])
exp(-1*model02$coefficients[1]+model02$coefficients[4]*0)
exp(-1*model02$coefficients[1]+model02$coefficients[4]*-1)
exp(-1*model02$coefficients[1]+model02$coefficients[4]*-2)

# probability:
exp(-1*model02$coefficients[1]+model02$coefficients[4]*1)/(1+exp(-1*model02$coefficients[1]+model02$coefficients[4]*1))
exp(-1*model02$coefficients[1]+model02$coefficients[4]*0)/(1+exp(-1*model02$coefficients[1]+model02$coefficients[4]*0))
exp(-1*model02$coefficients[1]+model02$coefficients[4]*-1)/(1+exp(-1*model02$coefficients[1]+model02$coefficients[4]*-1))
exp(-1*model02$coefficients[1]+model02$coefficients[4]*-2)/(1+exp(-1*model02$coefficients[1]+model02$coefficients[4]*-2))

# MODEL 03: TESTING OUT INTERACTIONS

model03 = clm(formula = LapplyF ~ 1 + PARED + PUBLIC + GPA3 + PARED*GPA3 +
                PUBLIC*GPA3 + PARED*PUBLIC + PARED*PUBLIC*GPA3, data = data01, control = clm.control(trace = 1))

summary(model03)
anova(model02, model03)

