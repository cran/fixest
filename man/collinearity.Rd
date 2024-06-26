% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/miscfuns.R
\name{collinearity}
\alias{collinearity}
\title{Collinearity diagnostics for \code{fixest} objects}
\usage{
collinearity(x, verbose)
}
\arguments{
\item{x}{A \code{fixest} object obtained from, e.g. functions \code{\link{femlm}}, \code{\link{feols}} or \code{\link{feglm}}.}

\item{verbose}{An integer. If higher than or equal to 1, then a note is prompted at
each step of the algorithm. By default \code{verbose = 0} for small problems
and to 1 for large problems.}
}
\value{
It returns a text message with the identified diagnostics.
}
\description{
In some occasions, the optimization algorithm of \code{\link{femlm}} may fail to converge, or
the variance-covariance matrix may not be available. The most common reason of why
this happens is collinearity among variables. This function helps to find out which
set of variables is problematic.
}
\details{
This function tests: 1) collinearity with the fixed-effect variables,
2) perfect multi-collinearity between the variables, 3) perfect multi-collinearity
between several variables and the fixed-effects, and 4) identification issues
when there are non-linear in parameters parts.
}
\examples{

# Creating an example data base:
set.seed(1)
fe_1 = sample(3, 100, TRUE)
fe_2 = sample(20, 100, TRUE)
x = rnorm(100, fe_1)**2
y = rnorm(100, fe_2)**2
z = rnorm(100, 3)**2
dep = rpois(100, x*y*z)
base = data.frame(fe_1, fe_2, x, y, z, dep)

# creating collinearity problems:
base$v1 = base$v2 = base$v3 = base$v4 = 0
base$v1[base$fe_1 == 1] = 1
base$v2[base$fe_1 == 2] = 1
base$v3[base$fe_1 == 3] = 1
base$v4[base$fe_2 == 1] = 1

# Estimations:

# Collinearity with the fixed-effects:
res_1 = femlm(dep ~ log(x) + v1 + v2 + v4 | fe_1 + fe_2, base)
collinearity(res_1)

# => collinearity with the first fixed-effect identified, we drop v1 and v2
res_1bis = femlm(dep ~ log(x) + v4 | fe_1 + fe_2, base)
collinearity(res_1bis)

# Multi-Collinearity:
res_2 =  femlm(dep ~ log(x) + v1 + v2 + v3 + v4, base)
collinearity(res_2)


}
\author{
Laurent Berge
}
