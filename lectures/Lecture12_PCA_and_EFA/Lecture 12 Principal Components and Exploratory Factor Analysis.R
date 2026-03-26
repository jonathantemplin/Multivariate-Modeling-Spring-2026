#IMPORT DATA AND PUT INTO DATASET
data01 = read.csv(file="Lecture 13 Data a.csv",header=TRUE)
data02 = read.csv(file="Lecture 13 Data b.csv",header=TRUE)

#AUTOMATING PACKAGES NEEDED FOR ANALYSES--------------------------------------------------------------------
haspackage = require("lavaan")
if (haspackage==FALSE){
  install.packages("lavaan")
}
library(lavaan)

#Advanced Matrix Operations --------------------------------------------------------------------------------

#correlation matrix of SAT data
sat_corrmat = cor(data01)

#eigenvalues and eigenvectors of correlation matrix:
sat_eigen = eigen(x = sat_corrmat, symmetric = TRUE)

sat_eigen$values

sat_eigen$vectors

#demonstration of spectral decomposition
corr_rank1 = sat_eigen$values[1] * sat_eigen$vectors[,1] %*% t(sat_eigen$vectors[,1])
corr_rank1

corr_rank2 = corr_rank1 + sat_eigen$values[2] * sat_eigen$vectors[,2] %*% t(sat_eigen$vectors[,2])
corr_rank2

#Principal Components Analysis of SAT data ---------------------------------------------------------------
#PCA of Correlation matrix (scale. = TRUE)
sat_pca_corr = prcomp(x = data01, scale. = TRUE)

#show the results (compare to eigenvalues/eigenvectors)
sat_pca_corr

#show the summary statistics
summary(sat_pca_corr)

#create same analysis but with covariance matrix (for visual) scale.=FALSE (covariance matrix) 
sat_pca_cov = prcomp(x = data01, scale. = FALSE)

#create augmented data matrix for plot
data01a = data01
data01a$type = "Raw"

data01b = data.frame(SATV = sat_pca_cov$x[,1], SATM = sat_pca_cov$x[,2], type="PC")
data01c = rbind(data01a, data01b)

plot(x = data01c$SATV, y = data01c$SATM, ylab = "SATM/PC2", xlab = "SATV/PC1", cex.main=1.5, frame.plot=FALSE, col=ifelse(data01c$type=="Raw", "red", "blue"))
legend(0, 400, pch=1, col=c("red", "blue"), c("Data", "PCs"), bty="o",  box.col="darkgreen", cex=1.5)

#Principal Components analysis of Gambling Data------------------------------------------------------------
#listwise removal of missing data (common in PCA -- but still a problem)
data02a = data02[which(is.na(data02$X1)==FALSE & is.na(data02$X3)==FALSE & is.na(data02$X5)==FALSE & is.na(data02$X9)==FALSE & is.na(data02$X10)==FALSE &
                       is.na(data02$X13)==FALSE & is.na(data02$X14)==FALSE & is.na(data02$X18)==FALSE & is.na(data02$X21)==FALSE & is.na(data02$X23)==FALSE),]

#analysis of covariance matrix of gambling data items
gambling_pca_cov = prcomp(x = data02a, scale. = FALSE) 
gambling_pca_cov
summary(gambling_pca_cov)

prop_var = t(summary(gambling_pca_cov)$importance[2:3,])
#creating a scree plot and a proportion of variance plot

par(mfrow = c(1,2))
plot(gambling_pca_cov, type="l", main = "Scree Plot of PCA Eigenvalues", lwd = 5)
matplot(prop_var, type="l", main = "Proportion of Variance Explained by Component", lwd = 5)
legend(x=5, y=.5, legend = c("Component Variance", "Cumulative Variance"), lty = 1:2, lwd=5, col=1:2)

#new variables from PCA:
View(gambling_pca_cov$x)

#EFA of Gambling Data with crazy constraints ------------------------------------------------------------------------------------

#step 1: determine number of factors in data

#one-factor model
EFA_1factor = factanal(x = data02a, factors = 1, rotation = "none")
EFA_1factor

#two-factor model
EFA_2factor = factanal(x = data02a, factors = 2, rotation = "none")
EFA_2factor

#constraint demonstration (lambda^T psi-1 lambda = diag)
Lambda = matrix(EFA_2factor$loadings, ncol=2)
Psi = diag(EFA_2factor$uniquenesses)

t(Lambda) %*% solve(Psi) %*% Lambda

#three-factor model
EFA_3factor = factanal(x = data02a, factors = 3, rotation = "none")
EFA_3factor

#constraint demonstration (lambda^T psi-1 lambda = diag)
Lambda = matrix(EFA_3factor$loadings, ncol=3)
Psi = diag(EFA_3factor$uniquenesses)

t(Lambda) %*% solve(Psi) %*% Lambda

#four-factor model
EFA_4factor = factanal(x = data02a, factors = 4, rotation = "none")
EFA_4factor

#constraint demonstration (lambda^T psi-1 lambda = diag)
Lambda = matrix(EFA_4factor$loadings, ncol=4)
Psi = diag(EFA_4factor$uniquenesses)

t(Lambda) %*% solve(Psi) %*% Lambda

#step 2: interpret the factors for final solution (4-factor model)

#varimax rotation
EFA_4factor_varimax = factanal(x = data02a, factors = 4, rotation = "varimax")
EFA_4factor_varimax

#promax rotation
EFA_4factor_varimax = factanal(x = data02a, factors = 4, rotation = "promax")
EFA_4factor_varimax

#CFA version of EFA using lavaan ---------------------------------------------------------------------------------------

#NOTE: THIS VERSION USES DATA WHERE ANY INCOMPLETE OBSERVATIONS ARE REMOVED TO MATCH NUMBERS (EXAMPLE PURPOSES ONLY)
#IN PRACTICE: DO NOT USE LISTWISE DELETION AS ML WILL STILL WORK!

#step #1: determining number of factors

#one factor CFA
CFA_1factor.syntax = "
factor1 =~ X1 + X3 + X5 + X9 + X10 + X13 + X14 + X18 + X21 + X23
"

#for comparison with EFA we are using standardized factors (var = 1; mean = 0)
CFA_1factor.model = cfa(model = CFA_1factor.syntax, data = data02a, estimator = "MLR", std.lv = TRUE)
summary(CFA_1factor.model, fit.measures = TRUE, standardized = TRUE)

EFA_1factor

#two factor CFA: one item removed from factor 2 and zero covariance between factors

CFA_2factor.syntax = "
factor1 =~ X1 + X3 + X5 + X9 + X10 + X13 + X14 + X18 + X21 + X23
factor2 =~      X3 + X5 + X9 + X10 + X13 + X14 + X18 + X21 + X23

factor1 ~ 0*factor2
"

#for comparison with EFA we are using standardized factors (var = 1; mean = 0)
CFA_2factor.model = cfa(model = CFA_2factor.syntax, data = data02a, estimator = "MLR", std.lv = TRUE)
summary(CFA_2factor.model, fit.measures = TRUE, standardized = TRUE)

EFA_2factor

#three factor CFA: 
CFA_3factor.syntax = "
factor1 =~ X1 + X3 + X5 + X9 + X10 + X13 + X14 + X18 + X21 + X23
factor2 =~      X3 + X5 + X9 + X10 + X13 + X14 + X18 + X21 + X23
factor3 =~           X5 + X9 + X10 + X13 + X14 + X18 + X21 + X23

factor1 ~ 0*factor2 + 0*factor3
factor2 ~ 0*factor3
"

#for comparison with EFA we are using standardized factors (var = 1; mean = 0)
CFA_3factor.model = cfa(model = CFA_3factor.syntax, data = data02a, estimator = "MLR", std.lv = TRUE)
summary(CFA_3factor.model, fit.measures = TRUE, standardized = TRUE)

EFA_3factor


#fit is not significantly worse...can stop here!

#re-examining three factor estimates
summary(CFA_3factor.model, fit.measures = TRUE, standardized = TRUE)

#NO FACTOR LOADINGS ON FACTOR 2 OR FACTOR 3 ARE SIGNIFICANTLY DIFFERENT FROM ZERO -- SO WE DON'T HAVE FACTORS -- THE ONE FACTOR MODEL IS BEST!

