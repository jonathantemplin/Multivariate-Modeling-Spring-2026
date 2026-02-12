if (!require(EPSY905R)){
  if (!require(devtools)) install.packages("devtools")
  devtools::install_github("jonathantemplin/EPSY905R")
}
library(EPSY905R)

if (!require(mvtnorm)){
  install.packages("mvtnorm")
}
library(mvtnorm)

#Playing with Matrices -------------------------------------------
#IMPORT DATA AND PUT INTO DATASET
data("dataSAT")
data01 = dataSAT

#defining the matrix X
X = cbind(data01$SATV,data01$SATM, (data01$SATV+data01$SATM)) #cbind stands for column-bind -- makes into a matrix
t(X) %*% X
solve(t(X) %*% X)
solve(cor(X))
y = rnorm(1000)
summary(lm(y ~ X))
#multivariate statistics -------------------------------

#calculating the mean vector:
N = (1/length(X[,1]))[1]
ONES = matrix(1,length(X[,1]),1)

XBAR = N*t(X)%*%ONES
XBAR

#calculating the covariance matrix:
S = N*t(X-ONES%*%t(XBAR))%*%(X-ONES%*%t(XBAR))
S

#diagonal matrix of standard deviations:
D = sqrt(diag(diag(S)))
D
Dinv = solve(D)
Dinv

R2 = Dinv%*%S%*%Dinv
R2
D %*% R2 %*% D

cor(X)

#Generalized Sample Variance
gsv = det(S)
gsv

#Total Sample Variance
tsv = sum(diag(S))
tsv

#Multivariate Normal Distribution PDF -----------------------------------------
XBAR
S

#use dmvnorm() function to return likelihood and log-likelihood from MVN:
likelihood_case631 = dmvnorm(x=X[631,],mean = XBAR,sigma = S, log=FALSE)
likelihood_case631

loglikelihood_case631 = dmvnorm(x=X[631,],mean = XBAR,sigma = S, log=TRUE)
loglikelihood_case631

likelihood_case717 = dmvnorm(x=X[717,], mean = XBAR, sigma = S, log=FALSE)
likelihood_case717

loglikelihood_case717 = dmvnorm(x=X[717,], mean = XBAR, sigma = S, log=TRUE)
loglikelihood_case717

likelihood_caseXBAR = dmvnorm(x=t(XBAR), mean = XBAR, sigma = S, log=FALSE)
likelihood_caseXBAR

loglikelihood_caseXBAR = dmvnorm(x=t(XBAR), mean = XBAR, sigma = S, log=TRUE)
loglikelihood_caseXBAR

