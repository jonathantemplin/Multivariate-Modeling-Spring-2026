# ============================================================
# 14 Missing Data.R
# Standalone R script extracted from 14 Missing Data.qmd
# ============================================================

# ---- motivating-example ----
set.seed(2024)
n = 400
x = rnorm(n, mean = 50, sd = 10)
y = 2 + 0.6 * x + rnorm(n, sd = 8)
df_full = data.frame(x = x, y = y)

# Introduce MNAR: high-y values are more likely to be missing
miss_prob = plogis((y - mean(y)) / sd(y))
df_mnar = df_full
df_mnar$y[runif(n) < miss_prob * 0.7] = NA

par(mfrow = c(1, 2), mar = c(4, 4, 2, 1))

plot(df_full$x, df_full$y,
     pch = 16, col = "#2980b9", cex = 0.7,
     xlab = "X", ylab = "Y", main = "Complete Data")
abline(lm(y ~ x, data = df_full), col = "#e74c3c", lwd = 2)

obs_idx = !is.na(df_mnar$y)
plot(df_mnar$x[obs_idx], df_mnar$y[obs_idx],
     pch = 16, col = "#2980b9", cex = 0.7,
     xlab = "X", ylab = "Y", main = "After Ignoring Missing (MNAR)")
abline(lm(y ~ x, data = df_mnar), col = "#e74c3c", lwd = 2)
abline(lm(y ~ x, data = df_full), col = "#7f8c8d", lwd = 2, lty = 2)
legend("topleft", legend = c("Observed fit", "True fit"),
       col = c("#e74c3c", "#7f8c8d"), lty = c(1, 2), lwd = 2, cex = 0.8)


# ---- mcar-demo ----
set.seed(42)
n = 300
x = rnorm(n)
y = 1.5 * x + rnorm(n)
df = data.frame(x = x, y = y)

# MCAR: randomly remove 30% of y values, unrelated to anything
df_mcar = df
df_mcar$y[sample(1:n, size = round(0.3 * n))] = NA

cat("Proportion missing:", mean(is.na(df_mcar$y)), "\n")
cat("Mean of x for observed y:  ", round(mean(df_mcar$x[!is.na(df_mcar$y)]), 3), "\n")
cat("Mean of x for missing y:   ", round(mean(df_mcar$x[is.na(df_mcar$y)]), 3), "\n")


# ---- mar-demo ----
set.seed(42)
n = 300
x = rnorm(n)
y = 1.5 * x + rnorm(n)
df = data.frame(x = x, y = y)

# MAR: missingness in y depends on x (observed), not on y itself
df_mar = df
miss_prob_mar = plogis(1.5 * df$x)   # higher x → more likely y is missing
df_mar$y[runif(n) < miss_prob_mar] = NA

cat("Proportion missing:", round(mean(is.na(df_mar$y)), 3), "\n")
cat("Mean of x for observed y:  ", round(mean(df_mar$x[!is.na(df_mar$y)]), 3), "\n")
cat("Mean of x for missing y:   ", round(mean(df_mar$x[is.na(df_mar$y)]), 3), "\n")


# ---- mnar-demo ----
set.seed(42)
n = 300
x = rnorm(n)
y = 1.5 * x + rnorm(n)
df = data.frame(x = x, y = y)

# MNAR: missingness in y depends on y itself
df_mnar = df
miss_prob_mnar = plogis(1.5 * df$y)   # higher y → more likely y is missing
df_mnar$y[runif(n) < miss_prob_mnar] = NA

cat("Proportion missing:", round(mean(is.na(df_mnar$y)), 3), "\n")
cat("Mean of OBSERVED y:  ", round(mean(df_mnar$y, na.rm = TRUE), 3), "\n")
cat("Mean of TRUE y:      ", round(mean(df$y), 3), "\n")


# ---- mechanism-summary ----
par(mfrow = c(1, 3), mar = c(4, 4, 3, 1))
set.seed(42)
n = 200
x_vals = rnorm(n)
y_vals = 1.5 * x_vals + rnorm(n)

# MCAR
obs_mcar = sample(c(TRUE, FALSE), n, replace = TRUE, prob = c(0.7, 0.3))
plot(x_vals, y_vals, pch = ifelse(obs_mcar, 16, 4),
     col = ifelse(obs_mcar, "#2980b9", "#e74c3c"),
     main = "MCAR", xlab = "X", ylab = "Y", cex = 0.8)
legend("topleft", pch = c(16, 4), col = c("#2980b9", "#e74c3c"),
       legend = c("Observed", "Missing"), cex = 0.75)

# MAR
miss_mar = runif(n) < plogis(1.5 * x_vals)
obs_mar = !miss_mar
plot(x_vals, y_vals, pch = ifelse(obs_mar, 16, 4),
     col = ifelse(obs_mar, "#2980b9", "#e74c3c"),
     main = "MAR (missing when X is high)", xlab = "X", ylab = "Y", cex = 0.8)

# MNAR
miss_mnar = runif(n) < plogis(1.5 * y_vals)
obs_mnar = !miss_mnar
plot(x_vals, y_vals, pch = ifelse(obs_mnar, 16, 4),
     col = ifelse(obs_mnar, "#2980b9", "#e74c3c"),
     main = "MNAR (missing when Y is high)", xlab = "X", ylab = "Y", cex = 0.8)


# ---- listwise ----
set.seed(42)
df_example = data.frame(
  id  = 1:10,
  x1  = c(2.1, 3.5, NA, 1.8, 4.2, 2.9, NA, 3.1, 2.7, 4.0),
  x2  = c(1.0, NA, 2.2, 1.5, NA, 2.8, 1.9, NA, 2.3, 1.7),
  y   = c(5.1, 6.8, 4.9, 5.5, 7.2, 6.1, 5.8, 6.4, 5.7, 7.0)
)

complete_cases = df_example[complete.cases(df_example), ]
cat("Original N:", nrow(df_example), "\n")
cat("After listwise deletion N:", nrow(complete_cases), "\n")
cat("Percent retained:", round(100 * nrow(complete_cases) / nrow(df_example), 1), "%\n")


# ---- pairwise ----
set.seed(42)
n = 100
x1 = rnorm(n)
x2 = 0.5 * x1 + rnorm(n)
x3 = 0.4 * x2 + rnorm(n)

df_pw = data.frame(x1 = x1, x2 = x2, x3 = x3)
df_pw$x2[sample(1:n, 20)] = NA
df_pw$x3[sample(1:n, 25)] = NA

# Pairwise correlation (default in cor() with use = "pairwise.complete.obs")
cor_pairwise = cor(df_pw, use = "pairwise.complete.obs")
round(cor_pairwise, 3)


# ---- mean-imputation ----
set.seed(42)
n = 200
y_true = rnorm(n, mean = 5, sd = 2)
y_obs = y_true
y_obs[sample(1:n, 60)] = NA   # 30% missing (MCAR for this demo)

y_mean_imp = y_obs
y_mean_imp[is.na(y_obs)] = mean(y_obs, na.rm = TRUE)

cat("True SD:         ", round(sd(y_true), 3), "\n")
cat("Observed SD:     ", round(sd(y_obs, na.rm = TRUE), 3), "\n")
cat("After mean imp SD:", round(sd(y_mean_imp), 3), "\n")


# ---- single-imputation ----
set.seed(42)
n = 200
x = rnorm(n)
y = 1.5 * x + rnorm(n, sd = 1.5)
df_si = data.frame(x = x, y = y)
df_si$y[sample(1:n, 60)] = NA

# Fit regression on observed cases
reg_model = lm(y ~ x, data = df_si, na.action = na.omit)
# Impute missing
df_si$y_imp = df_si$y
missing_idx = is.na(df_si$y)
df_si$y_imp[missing_idx] = predict(reg_model,
                                    newdata = df_si[missing_idx, ])

cat("True residual SD:    ", round(sd(y - (1.5 * x)), 3), "\n")
cat("Imputed residual SD: ", round(sd(df_si$y_imp - predict(lm(y_imp ~ x, data = df_si))), 3), "\n")


# ---- simulate-data ----
set.seed(2025)
n = 500

# Exogenous predictor
x1 = rnorm(n, mean = 0, sd = 1)

# Mediator: caused by x1
x2 = 0.50 * x1 + rnorm(n, sd = sqrt(1 - 0.50^2))

# Outcome: caused by both x1 and x2
y  = 0.30 * x1 + 0.55 * x2 + rnorm(n, sd = sqrt(1 - 0.30^2 - 0.55^2 - 2*0.30*0.55*0.50))

df_full = data.frame(x1 = x1, x2 = x2, y = y)

cat("True path coefficients:\n")
cat("  x2 ~ x1 : 0.50\n")
cat("  y  ~ x1 : 0.30\n")
cat("  y  ~ x2 : 0.55\n")
cat("N =", nrow(df_full), "| No missing values\n")


# ---- introduce-missingness ----
set.seed(2025)
df_miss = df_full

# y is missing when x1 is high (MAR: depends on observed x1)
prob_miss_y = plogis(1.2 * df_full$x1)
df_miss$y[runif(n) < prob_miss_y * 0.55] = NA

# x2 is missing when x1 is low (MAR: depends on observed x1)
prob_miss_x2 = plogis(-1.0 * df_full$x1)
df_miss$x2[runif(n) < prob_miss_x2 * 0.40] = NA

cat("Missing in y: ", sum(is.na(df_miss$y)),
    "(", round(100 * mean(is.na(df_miss$y)), 1), "%)\n")
cat("Missing in x2:", sum(is.na(df_miss$x2)),
    "(", round(100 * mean(is.na(df_miss$x2)), 1), "%)\n")
cat("Complete cases:", sum(complete.cases(df_miss)),
    "(", round(100 * mean(complete.cases(df_miss)), 1), "%)\n")


# ---- path-model ----
library(lavaan)

path_model = "
  x2 ~ x1
  y  ~ x1 + x2
"

# Complete-case analysis (lavaan default drops missing rows)
fit_cc = sem(path_model, data = df_miss, missing = "listwise")

# FIML
fit_fiml = sem(path_model, data = df_miss, missing = "fiml")


# ---- path-results ----
results_cc   = parameterEstimates(fit_cc,   standardized = FALSE)
results_fiml = parameterEstimates(fit_fiml, standardized = FALSE)

# Extract regression paths only
paths_cc   = results_cc[results_cc$op == "~",
                         c("lhs", "op", "rhs", "est", "se")]
paths_fiml = results_fiml[results_fiml$op == "~",
                           c("lhs", "op", "rhs", "est", "se")]

paths_cc$method   = "Complete Case"
paths_fiml$method = "FIML"

comparison = rbind(paths_cc, paths_fiml)
comparison$true_value = c(0.50, 0.30, 0.55, 0.50, 0.30, 0.55)
comparison$bias = round(comparison$est - comparison$true_value, 4)
comparison[, c("lhs", "rhs", "method", "true_value", "est", "se", "bias")]


# ---- fiml-visual ----
path_labels = c("x2~x1 (true=0.50)", "y~x1 (true=0.30)", "y~x2 (true=0.55)")
true_vals   = c(0.50, 0.30, 0.55)

cc_est   = paths_cc$est
cc_se    = paths_cc$se
fi_est   = paths_fiml$est
fi_se    = paths_fiml$se

y_pos = c(3, 2, 1)
par(mar = c(5, 10, 3, 2))
plot(NA, xlim = c(-0.1, 1.0), ylim = c(0.5, 3.5),
     yaxt = "n", xlab = "Path Coefficient Estimate", ylab = "",
     main = "Complete Case vs. FIML Path Estimates")

axis(2, at = y_pos, labels = path_labels, las = 1, cex.axis = 0.85)
abline(v = true_vals, col = "#27ae60", lty = 2, lwd = 1.5)

offset = 0.15
for (i in 1:3) {
  # Complete case
  points(cc_est[i], y_pos[i] + offset, pch = 16, col = "#e74c3c", cex = 1.3)
  lines(c(cc_est[i] - 1.96 * cc_se[i], cc_est[i] + 1.96 * cc_se[i]),
        c(y_pos[i] + offset, y_pos[i] + offset), col = "#e74c3c", lwd = 2)

  # FIML
  points(fi_est[i], y_pos[i] - offset, pch = 17, col = "#2980b9", cex = 1.3)
  lines(c(fi_est[i] - 1.96 * fi_se[i], fi_est[i] + 1.96 * fi_se[i]),
        c(y_pos[i] - offset, y_pos[i] - offset), col = "#2980b9", lwd = 2)
}

legend("topright", pch = c(16, 17), col = c("#e74c3c", "#2980b9"),
       legend = c("Complete Case", "FIML"), cex = 0.9)
legend("topleft", lty = 2, col = "#27ae60",
       legend = "True value", cex = 0.9)


# ---- mice-setup ----
library(mice)

# Use the same MAR dataset from the path analysis section
cat("Variables with missing data:\n")
print(colSums(is.na(df_miss)))

# Run MICE with m = 20 imputed datasets, PMM for continuous variables
imp = mice(df_miss,
           m       = 20,        # number of imputed datasets
           method  = "pmm",     # predictive mean matching
           maxit   = 10,        # iterations per imputation
           seed    = 2025,
           printFlag = FALSE)

summary(imp)


# ---- mice-convergence ----
plot(imp, layout = c(2, 2))


# ---- mice-analysis ----
# Fit the path-like model (as a regression, for simplicity) on each imputed dataset

# Model 1: x2 ~ x1
fit_x2 = with(imp, lm(x2 ~ x1))

# Model 2: y ~ x1 + x2
fit_y  = with(imp, lm(y ~ x1 + x2))

# Pool using Rubin's Rules
pooled_x2 = pool(fit_x2)
pooled_y  = pool(fit_y)

cat("=== Pooled: x2 ~ x1 ===\n")
summary(pooled_x2)[, c("term", "estimate", "std.error", "statistic", "p.value")]


# ---- mice-pooled-y ----
cat("=== Pooled: y ~ x1 + x2 ===\n")
print(summary(pooled_y)[, c("term", "estimate", "std.error", "statistic", "p.value")])

cat("\n=== Fraction of Missing Information (FMI) ===\n")
print(pooled_y$pooled[, c("term", "fmi", "lambda")])


# ---- three-way ----
# True values
true_b = c("x2~x1" = 0.50, "y~x1" = 0.30, "y~x2" = 0.55)

# Complete case
cc_x2 = coef(lm(x2 ~ x1, data = df_miss[complete.cases(df_miss), ]))
cc_y  = coef(lm(y  ~ x1 + x2, data = df_miss[complete.cases(df_miss), ]))

# MICE pooled
mx2 = summary(pooled_x2)
my  = summary(pooled_y)

results_df = data.frame(
  Path       = c("x2 ~ x1", "y ~ x1", "y ~ x2"),
  True       = c(0.50, 0.30, 0.55),
  CC         = round(c(cc_x2["x1"], cc_y["x1"], cc_y["x2"]), 3),
  FIML       = round(c(paths_fiml$est[1], paths_fiml$est[2], paths_fiml$est[3]), 3),
  MICE       = round(c(mx2$estimate[mx2$term=="x1"],
                       my$estimate[my$term=="x1"],
                       my$estimate[my$term=="x2"]), 3)
)

results_df$Bias_CC   = round(results_df$CC   - results_df$True, 3)
results_df$Bias_FIML = round(results_df$FIML - results_df$True, 3)
results_df$Bias_MICE = round(results_df$MICE - results_df$True, 3)

print(results_df)
