## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE, eval = TRUE)
Sys.setenv(lang = "en")

## -----------------------------------------------------------------------------
# Data generation
set.seed(0)
N = 20 ; n_id = N/5; n_time = N/n_id
base = data.frame(y = rnorm(N), x = rnorm(N), id = rep(1:n_id, n_time), 
                  time = rep(1:n_time, each = n_id))


## -----------------------------------------------------------------------------
library(fixest) ; library(sandwich)

# Estimations
res_lm    = lm(y ~ x, base)
res_feols = feols(y ~ x, base)

# Same standard-errors
rbind(se(res_lm), se(res_feols))

# Heteroskedasticity-robust covariance
se_lm_white    = sqrt(diag(vcovHC(res_lm, type = "HC1")))
se_feols_white = se(res_feols, se = "white")
rbind(se_lm_white, se_feols_white)

## ---- echo = FALSE------------------------------------------------------------
if(!requireNamespace("plm", quietly = TRUE)){
    knitr::opts_chunk$set(eval = FALSE)
    cat("Evaluation of the next chunks requires 'plm', which is not present.")
}

## -----------------------------------------------------------------------------
library(plm)

# Estimations
est_lm    = lm(y ~ x + as.factor(id) + as.factor(time), base)
est_plm   = plm(y ~ x + as.factor(time), base, index = c("id", "time"), model = "within")
est_feols = feols(y ~ x | id + time, base)

#
# "Standard" standard-errors
#

# By default fixest clusters the SEs when FEs are present,
#  so we need to ask for standard SEs explicitly.
rbind(se(est_lm)["x"], se(est_plm)["x"], se(est_feols, se = "standard"))

# p-values:
rbind(pvalue(est_lm)["x"], pvalue(est_plm)["x"], pvalue(est_feols, se = "standard"))


## -----------------------------------------------------------------------------
# Clustered by id
se_lm_id    = sqrt(vcovCL(est_lm, cluster = base$id, type = "HC1")["x", "x"])
se_plm_id   = sqrt(vcovHC(est_plm, cluster = "group")["x", "x"])
se_stata_id = 0.165385      # vce(cluster id)
se_feols_id = se(est_feols) # By default: clustered according to id

rbind(se_lm_id, se_plm_id, se_stata_id, se_feols_id)

## -----------------------------------------------------------------------------
rbind(r = coeftable(est_feols), stata = coeftable(est_feols, dof = dof(t.df = "min")))

## -----------------------------------------------------------------------------
# How to get the lm version
se_feols_id_lm = se(est_feols, dof = dof(fixef.K = "full"))
rbind(se_lm_id, se_feols_id_lm)

# How to get the plm version
se_feols_id_plm = se(est_feols, dof = dof(fixef.K = "none", cluster.adj = FALSE))
rbind(se_plm_id, se_feols_id_plm)

## ---- echo = FALSE------------------------------------------------------------
if(!requireNamespace("lfe", quietly = TRUE)){
    knitr::opts_chunk$set(eval = FALSE)
    cat("Evaluation of the next chunk requires 'lfe', which is not present.")
} else {
    knitr::opts_chunk$set(eval = TRUE)
}

## -----------------------------------------------------------------------------
library(lfe, quietly = TRUE, warn.conflicts = FALSE)

# lfe: clustered by id
est_lfe = felm(y ~ x | id + time | 0 | id, base)
se_lfe_id = se(est_lfe)

# The two are different, and it cannot be directly replicated by feols
rbind(se_lfe_id, se_feols_id)

# You have to provide a custom VCOV to replicate lfe's VCOV
my_vcov = vcov(est_feols, dof = dof(adj = FALSE))
se(est_feols, .vcov = my_vcov * 19/18) # Note that there are 20 observations

# Differently from feols, the SEs are different if time is not a FE:
# => now SEs are identical. (The warning is from lfe.)
rbind(se(felm(y ~ x + factor(time) | id | 0 | id, base))["x"],
      se(feols(y ~ x + factor(time) | id, base))["x"])

# Now with two-way clustered standard-errors
est_lfe_2way = felm(y ~ x | id + time | 0 | id + time, base)
se_lfe_2way = se(est_lfe_2way)
se_feols_2way = se(est_feols, se = "twoway")
rbind(se_lfe_2way, se_feols_2way)

# To have the same pvalues, you need to use t.df="min"
rbind(pvalue(est_lfe_2way), pvalue(est_feols, se = "twoway", dof = dof(t.df = "min")))

## -----------------------------------------------------------------------------
setFixest_dof(dof(cluster.df = "min", t.df = "min"))

## -----------------------------------------------------------------------------
setFixest_se(no_FE = "standard", one_FE = "standard", two_FE = "standard")

