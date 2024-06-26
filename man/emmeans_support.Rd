% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ctb_emmeans.R
\name{emmeans_support}
\alias{emmeans_support}
\title{Support for emmeans package}
\description{
If \pkg{emmeans} is installed, its functionality is supported for \code{fixest}
or \code{fixest_multi} objects. Its reference grid is based on the main part
of the model, and does not include fixed effects or instrumental variables.
Note that any desired arguments to \code{vcov()} may be passed as optional
arguments in \code{emmeans::emmeans()} or \code{emmeans::ref_grid()}.
}
\note{
When fixed effects are present, estimated marginal means (EMMs) are estimated
correctly, provided equal weighting is used. However, the SEs of these EMMs
will be incorrect - often dramatically - because the estimated variance of
the intercept is not available. However, \emph{contrasts} among EMMs can be
estimated and tested with no issues, because these do not involve the
intercept.
}
\examples{
if(requireNamespace("emmeans") && requireNamespace("AER")) {
    data(Fatalities, package = "AER")
    Fatalities$frate = with(Fatalities, fatal/pop * 10000)
    fat.mod = feols(frate ~ breath * jail * beertax | state + year, data = Fatalities)
    emm = emmeans::emmeans(fat.mod, ~ breath*jail, cluster = ~ state + year)
    emm   ### SEs and CIs are incorrect

    emmeans::contrast(emm, "consec", by = "breath")   ### results are reliable
}
}
\author{
Russell V. Lenth
}
