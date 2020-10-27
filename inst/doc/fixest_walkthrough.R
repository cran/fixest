## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

library(fixest)
setFixest_nthreads(1)

## ----echo=TRUE----------------------------------------------------------------
library(fixest)
data(trade)


## ---- echo=FALSE, results='asis'----------------------------------------------
tab = head(trade)
knitr::kable(tab)

## -----------------------------------------------------------------------------
gravity_pois <- feglm(Euros ~ log(dist_km)|Origin+Destination+Product+Year, trade)

## -----------------------------------------------------------------------------
print(gravity_pois)

## -----------------------------------------------------------------------------
summary(gravity_pois, se = "twoway")

## ---- eval = FALSE------------------------------------------------------------
#  # Equivalent ways of clustering the SEs:
#  # One-way clustering is deduced from the arguent 'cluster'
#  # - using the vector:
#  summary(gravity_pois, cluster = trade$Product)
#  # - by reference:
#  summary(gravity_pois, cluster = "Product")
#  # - with a formula:
#  summary(gravity_pois, cluster = ~Product)

## ---- eval = TRUE-------------------------------------------------------------
summary(gravity_pois, cluster = ~Product)

## -----------------------------------------------------------------------------
gravity_simple = feglm(Euros ~ log(dist_km), trade)
# Two way clustering is deduced from the argument 'cluster'
# Using data:
summary(gravity_simple, cluster = trade[, c("Origin", "Destination")])
# Using a formula (note that the values of the variables are 
#  fetched directly in the original database):
summary(gravity_simple, cluster = ~Origin+Destination)

## -----------------------------------------------------------------------------
gravity_ols <- feols(log(Euros) ~ log(dist_km)|Origin+Destination+Product+Year, trade)

## -----------------------------------------------------------------------------
gravity_negbin <- fenegbin(Euros ~ log(dist_km)|Origin+Destination+Product+Year, trade)


## ---- eval=FALSE--------------------------------------------------------------
#  etable(gravity_pois, gravity_negbin, gravity_ols,
#           se = "twoway", subtitles = c("Poisson", "Negative Binomial", "Gaussian"))

## ---- echo=FALSE, results='asis'----------------------------------------------
tab = etable(gravity_pois, gravity_negbin, gravity_ols, se = "twoway", subtitles = c("Poisson", "Negative Binomial", "Gaussian"))
# problem to display the second empty line in markdown
knitr::kable(tab[-2, ])

## -----------------------------------------------------------------------------
gravity_subfe = list()
all_FEs = c("Year", "Destination", "Origin")
for(i in 0:3){
	gravity_subfe[[i+1]] = feglm(Euros ~ log(dist_km), trade, fixef = all_FEs[0:i])
}

## ---- eval=FALSE--------------------------------------------------------------
#  etable(gravity_subfe, cluster = ~Origin+Destination)

## ---- echo=FALSE, results='asis'----------------------------------------------
tab = etable(gravity_subfe, cluster = ~Origin+Destination)
knitr::kable(tab)

## -----------------------------------------------------------------------------
# with two-way clustered SEs
etable(gravity_subfe, cluster = ~Origin+Destination, tex = TRUE)

## ---- eval=FALSE--------------------------------------------------------------
#  # we set the dictionary once and for all
#  myDict = c("log(dist_km)" = "$\\ln (Distance)$", "(Intercept)" = "Constant")
#  # 1st export: we change the signif code and drop the intercept
#  etable(gravity_subfe, signifCode = c("a" = 0.01, "b" = 0.05),
#         drop = "Const", dict = myDict, file = "Estimation Tables.tex",
#         replace = TRUE, title = "First export -- normal Standard-errors")
#  # 2nd export: clustered S-E + distance as the first coefficient
#  etable(gravity_subfe, cluster = ~Product, order = "Dist",
#         dict = myDict, file = "Estimation Tables.tex",
#         title = "Second export -- clustered standard-errors (on Product variable)")
#  

## -----------------------------------------------------------------------------
fixedEffects <- fixef(gravity_pois)
summary(fixedEffects)

## -----------------------------------------------------------------------------
fixedEffects$Year

## ---- fig.width=7-------------------------------------------------------------
plot(fixedEffects)

## -----------------------------------------------------------------------------
base_vs = iris
names(base_vs) = c(paste0("x", 1:4), "species")

## -----------------------------------------------------------------------------
est_vs = feols(x1 ~ x2 | species[x3], base_vs)
est_vs

## -----------------------------------------------------------------------------
summary(fixef(est_vs))

## -----------------------------------------------------------------------------
# we create another "fixed-effect"
base_vs$fe = rep(1:5, 30)
head(base_vs)

## -----------------------------------------------------------------------------
est_comb = feols(x1 ~ x2 | species^fe, base_vs)
est_comb

## -----------------------------------------------------------------------------
fixef(est_comb)[[1]]

## ---- eval = TRUE-------------------------------------------------------------
# Sample data illustrating the DiD
data(base_did)
head(base_did)

## ---- eval = TRUE-------------------------------------------------------------
# Estimation of yearly effect
# We also add individual/time fixed-effects:
est_did = feols(y ~ x1 + i(treat, period, 5) | id + period, base_did)
est_did

## ---- fig.width=7-------------------------------------------------------------
coefplot(est_did)

## ---- eval = TRUE-------------------------------------------------------------
# Estimation of yearly effect
# We also add individual/time fixed-effects:
est_f_did = feols(y ~ x1 + i(period, keep = 3:6) + i(treat, period, 5) | id, base_did)
# The display in etable is now 'nicer' than when using regular factors
etable(est_f_did, dict = c("period" = "Period", "6" = "six"))

## ---- fig.width=7-------------------------------------------------------------
coefplot(est_f_did, only.inter = FALSE)

## -----------------------------------------------------------------------------
est1 = feols(y ~ l(x1, 0:1), base_did, panel.id = ~id+period)
est2 = feols(f(y) ~ l(x1, -1:1), base_did, panel.id = ~id+period)
est3 = feols(l(y) ~ l(x1, 0:3), base_did, panel.id = ~id+period)
etable(est1, est2, est3, order = "f", drop = "Int")

## -----------------------------------------------------------------------------
# setting up the panel
pdat = panel(base_did, ~id + period)
# Now the panel.id argument is not required
est1 = feols(y ~ l(x1, 0:1), pdat)
est2 = feols(f(y) ~ l(x1, -1:1), pdat)
# You can use sub selections of the panel data
est_sub = feols(y ~ l(x1, 0:1), pdat[!pdat$period %in% c(2, 4)])
etable(est1, est2, est_sub, order = "f", drop = "Int")

## -----------------------------------------------------------------------------
library(data.table)
pdat_dt = panel(as.data.table(base_did), ~id+period)
# we create a lagged value of the variable x1
pdat_dt[, x1_l1 := l(x1)]
# Now 
pdat_dt[, c("x1_l1_fill0", "y_f2") := .(l(x1, fill = 0), f(y, 2))]
head(pdat_dt)

## -----------------------------------------------------------------------------
base_lag = base_did
# we create a lagged value of the variable x1
base_lag$x1.l1 = lag(x1 ~ id + period, 1, base_lag)
head(base_lag)

## -----------------------------------------------------------------------------
library(data.table)
base_lag_dt = as.data.table(base_did)
# we create a lagged value of the variable x1
base_lag_dt[, x1.l1 := lag(x1 ~ id + period, 1)]

## -----------------------------------------------------------------------------
# Generating data:
n = 1000
# x and y: two positive random variables
x = rnorm(n, 1, 5)**2
y = rnorm(n, -1, 5)**2
# E(z) = 2*x + 3*y and some noise
z = rpois(n, 2*x + 3*y) + rpois(n, 1)
base = data.frame(x, y, z)

## -----------------------------------------------------------------------------
result_NL = feNmlm(z~0, base, NL.fml = ~ log(a*x + b*y), NL.start = list(a=1, b=1), lower = list(a=0, b=0))

## -----------------------------------------------------------------------------
print(result_NL)

## -----------------------------------------------------------------------------
# the class of each observation
id = sample(20, n, replace = TRUE)
base$id = id
# the vector of fixed-effects
gamma = rnorm(20)**2
# the new vector z_bis
z_bis = rpois(n, gamma[id] * (2*x + 3*y)) + rpois(n, 1)
base$z_bis = z_bis

## -----------------------------------------------------------------------------
# we add the fixed-effect in the formula
result_NL_fe = feNmlm(z_bis~0|id, base, NL.fml = ~ log(2*x + b*y), NL.start = list(b=1), lower = list(b=0))
# The coef should be around 3
coef(result_NL_fe)
# the gamma and the exponential of the fixed-effects should be similar
rbind(gamma, exp(fixef(result_NL_fe)$id[as.character(1:20)]))


## ---- eval = FALSE------------------------------------------------------------
#  # Sample of results:
#  # 1 nthreads: 3.13s
#  system.time(fenegbin(Euros ~ log(dist_km)|Origin+Destination+Product+Year, trade, nthreads = 1))
#  # 2 nthreads: 1.82s
#  system.time(fenegbin(Euros ~ log(dist_km)|Origin+Destination+Product+Year, trade, nthreads = 2))
#  # 4 nthreads: 1.17s
#  system.time(fenegbin(Euros ~ log(dist_km)|Origin+Destination+Product+Year, trade, nthreads = 4))

