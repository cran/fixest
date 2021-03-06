% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/did.R
\name{sunab}
\alias{sunab}
\alias{sunab_att}
\title{Sun and Abraham interactions}
\usage{
sunab(cohort, period, ref.c = NULL, ref.p = -1, att = FALSE, no_agg = FALSE)

sunab_att(cohort, period, ref.c = NULL, ref.p = -1)
}
\arguments{
\item{cohort}{A vector representing the cohort. It should represent the period at which the treatment has been received (and thus be fixed for each unit).}

\item{period}{A vector representing the period. It can be either a relative time period (with negative values representing the before the treatment and positive values after the treatment), or a regular time period. In the latter case, the relative time period will be created from the cohort information (which represents the time at which the treatment has been received).}

\item{ref.c}{A vector of references for the cohort. By default the never treated cohorts are taken as reference and the always treated are excluded from the estimation. You can add more references with this argument, which means that dummies will not be created for them (but they will remain in the estimation).}

\item{ref.p}{A vector of references for the (relative!) period. By default the first relative period (RP) before the treatment, i.e. -1, is taken as reference. You can instead use your own references (i.e. RPs for which dummies will not be created -- but these observations remain in the sample). Please note that you will need at least two references. You can use the special variables \code{.F} and \code{.L} to access the first and the last relative periods.}

\item{att}{Logical, default is \code{FALSE}. If \code{TRUE}: then the total average treatment effect for the treated is computed (instead of the ATT for each relative period).}

\item{no_agg}{Logical, default is \code{FALSE}. If \code{TRUE}: then there is no aggregation, leading to the esimation of all \code{cohort x time to treatment} coefficients.}
}
\value{
If not used within a \code{fixest} estimation, this function will return a matrix of interacted coefficients.
}
\description{
User-level method to implement staggered difference-in-difference estimations a la Sun and Abraham (Journal of Econometrics, forthcoming).
}
\details{
This function creates a matrix of \code{cohort x relative_period} interactions, and if used within a \code{fixest} estimation, the coefficients will automatically be aggregated to obtain the ATT for each relative period. In practice, the coefficients are aggregated with the \code{\link[fixest]{aggregate.fixest}} function whose argument \code{agg} is automatically set to the appropriate value.

The SA method requires relative periods (negative/positive for before/after the treatment). Either the user can compute the RP (relative periods) by his/her own, either the RPs are computed on the fly from the periods and the cohorts (which then should represent the treatment period).

The never treated, which are the cohorts displaying only negative RPs are used as references (i.e. no dummy will be constructed for them). On the other hand, the always treated are removed from the estimation, by means of adding NAs for each of their observations.

If the RPs have to be constructed on the fly, any cohort that is not present in the period is considered as never treated. This means that if the period ranges from 1995 to 2005, \code{cohort = 1994} will be considered as never treated, although it should be considered as always treated: so be careful.

If you construct your own relative periods, the controls cohorts should have only negative RPs.
}
\examples{

# Simple DiD example
data(base_stagg)
head(base_stagg)

# Note that the year_treated is set to 1000 for the never treated
table(base_stagg$year_treated)
table(base_stagg$time_to_treatment)

# The DiD estimation
res_sunab = feols(y ~ x1 + sunab(year_treated, year), base_stagg)
etable(res_sunab)

# By default the reference periods are the first year and the year before the treatment
# i.e. ref.p = c(-1, .F); where .F is a shortcut for the first period.
# Say you want to set as references the first three periods on top of -1

res_sunab_3ref = feols(y ~ x1 + sunab(year_treated, year, ref.p = c(.F + 0:2, -1)), base_stagg)

# Display the two results
iplot(list(res_sunab, res_sunab_3ref))

# ... + show all refs
iplot(list(res_sunab, res_sunab_3ref), ref = "all")


#
# ATT
#

# To get the total ATT, you can use summary with the agg argument:
summary(res_sunab, agg = "ATT")

# You can also look at the total effect per cohort
summary(res_sunab, agg = "cohort")


}
