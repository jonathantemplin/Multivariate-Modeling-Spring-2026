# ============================================================
# Lecture 13: Classification Methods — Mixture Models
# Latent Class Analysis (LCA) & Latent Profile Analysis (LPA)
# ============================================================

# ---- Install libraries if not present ----------------------
required_packages <- c("poLCA", "ggplot2", "tidyLPA")
for (pkg in required_packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg)
  }
}

library(poLCA)
library(ggplot2)
library(tidyLPA)


# ============================================================
# LCA EXAMPLE: Macready & Dayton (1977)
# ============================================================

# ---- Data Preparation --------------------------------------
# Load the data (comma-separated, no header)
md_data <- read.csv("mddata.dat", header = FALSE)
colnames(md_data) <- c("u1", "u2", "u3", "u4")

# poLCA requires values starting at 1 (not 0)
# Recode: 0 -> 1 (incorrect) and 1 -> 2 (correct)
md_data <- md_data + 1

# Verify recoding
head(md_data)
table(md_data$u1)


# ---- Fit 2-Class LCA Model ---------------------------------
# Define the model formula: all items as indicators, no covariates
lca_formula <- cbind(u1, u2, u3, u4) ~ 1

# Fit 2-class LCA with 10 random starting points
set.seed(123)
lca_2class <- poLCA(lca_formula,
                    data    = md_data,
                    nclass  = 2,
                    nrep    = 10,
                    verbose = FALSE)

# Print a summary of results
print(lca_2class)


# ---- Chi-Squared Fit Tests ---------------------------------
# poLCA automatically computes chi-square tests in the model object
lca_2class$Chisq   # Pearson chi-square statistic
lca_2class$Gsq     # Likelihood ratio G^2 statistic
lca_2class$df      # Degrees of freedom

# Compute p-values
pchisq(lca_2class$Chisq, df = lca_2class$resid.df, lower.tail = FALSE)
pchisq(lca_2class$Gsq,   df = lca_2class$resid.df, lower.tail = FALSE)


# ---- Compare 1-, 2-, and 3-Class Models --------------------
set.seed(123)
lca_1class <- poLCA(lca_formula, data = md_data, nclass = 1,
                    nrep = 5,  verbose = FALSE)
lca_2class <- poLCA(lca_formula, data = md_data, nclass = 2,
                    nrep = 10, verbose = FALSE)
lca_3class <- poLCA(lca_formula, data = md_data, nclass = 3,
                    nrep = 10, verbose = FALSE)

# Compile fit statistics into a table
fit_table <- data.frame(
  Classes    = 1:3,
  Parameters = c(lca_1class$npar, lca_2class$npar, lca_3class$npar),
  LogL       = c(lca_1class$llik, lca_2class$llik, lca_3class$llik),
  AIC        = c(lca_1class$aic,  lca_2class$aic,  lca_3class$aic),
  BIC        = c(lca_1class$bic,  lca_2class$bic,  lca_3class$bic)
)
print(fit_table, digits = 2)


# ---- Compute Relative Entropy ------------------------------
# Relative entropy function for poLCA output
compute_entropy <- function(posterior) {
  N <- nrow(posterior)
  C <- ncol(posterior)
  posterior <- pmax(posterior, 1e-10)   # avoid log(0)
  entropy_raw <- -sum(posterior * log(posterior))
  relative_entropy <- 1 - entropy_raw / (N * log(C))
  return(relative_entropy)
}

# Apply to the 2-class model
entropy_2class <- compute_entropy(lca_2class$posterior)
cat("Relative Entropy (2-class):", round(entropy_2class, 3), "\n")


# ---- Visualize Item Response Profiles ----------------------
# Extract Pr(correct) = Pr(2) for each item and class
probs <- data.frame(
  item  = rep(c("u1", "u2", "u3", "u4"), 2),
  class = rep(c("Class 1 (Masters)", "Class 2 (Non-Masters)"), each = 4),
  prob  = c(lca_2class$probs$u1[1, 2],
            lca_2class$probs$u2[1, 2],
            lca_2class$probs$u3[1, 2],
            lca_2class$probs$u4[1, 2],
            lca_2class$probs$u1[2, 2],
            lca_2class$probs$u2[2, 2],
            lca_2class$probs$u3[2, 2],
            lca_2class$probs$u4[2, 2])
)

ggplot(probs, aes(x = item, y = prob, group = class, color = class)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 3) +
  scale_y_continuous(limits = c(0, 1), name = "P(Correct Response)") +
  labs(x = "Item", title = "LCA Item Response Profiles", color = "Class") +
  theme_bw(base_size = 14)


# ============================================================
# LPA EXAMPLE: Fisher's Iris Data
# ============================================================

# ---- Load Data and Fit Models ------------------------------
# Load the Iris data (space-separated, no header, no species label)
iris_dat <- read.table("iris.dat", header = FALSE)
colnames(iris_dat) <- c("x1", "x2", "x3", "x4")

# Fit LPA models with 2, 3, and 4 classes (Model 1: equal variances)
lpa_models <- estimate_profiles(iris_dat,
                                n_profiles  = 2:4,
                                variances   = "equal",
                                covariances = "zero")

# Compare fit statistics across models
get_fit(lpa_models)


# ---- Fit the 3-Class LPA Model -----------------------------
lpa_3class <- estimate_profiles(iris_dat,
                                n_profiles  = 3,
                                variances   = "equal",
                                covariances = "zero")

# View class-specific estimates
get_estimates(lpa_3class)

# Add posterior probabilities and class assignments to the data
lpa_3class_data <- get_data(lpa_3class)
head(lpa_3class_data)


# ---- Built-in Profile Plot ---------------------------------
plot_profiles(lpa_3class, add_line = TRUE, rawdata = FALSE)


# ---- Custom ggplot2 Profile Plot ---------------------------
# get_estimates() returns: Model, Classes, Class, Parameter, Estimate, SE, p, Item
ests <- get_estimates(lpa_3class)
ests_means <- ests[ests$Category == "Means", ]

# Label classes by species
ests_means$Class_label <- factor(ests_means$Class,
                                 levels = 1:3,
                                 labels = c("Class 1 (setosa)",
                                            "Class 2 (versicolor)",
                                            "Class 3 (virginica)"))

ggplot(ests_means, aes(x = Parameter, y = Estimate,
                       group = Class_label, color = Class_label)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 3) +
  labs(x = "Variable", y = "Mean",
       title = "LPA Profile Plot: 3-Class Solution",
       color = "Class") +
  theme_bw(base_size = 14)

