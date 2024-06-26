% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/methods.R
\name{nobs.fixest}
\alias{nobs.fixest}
\title{Extracts the number of observations form a \code{fixest} object}
\usage{
\method{nobs}{fixest}(object, ...)
}
\arguments{
\item{object}{A \code{fixest} object. Obtained using the functions \code{\link{femlm}}, \code{\link{feols}} or \code{\link{feglm}}.}

\item{...}{Not currently used.}
}
\value{
It returns an interger.
}
\description{
This function simply extracts the number of observations form a \code{fixest} object,
obtained using the functions \code{\link{femlm}}, \code{\link{feols}} or \code{\link{feglm}}.
}
\examples{

# simple estimation on iris data with "Species" fixed-effects
res = femlm(Sepal.Length ~ Sepal.Width + Petal.Length +
            Petal.Width | Species, iris)

nobs(res)
logLik(res)


}
\seealso{
See also the main estimation functions \code{\link{femlm}}, \code{\link{feols}} or \code{\link{feglm}}.
Use \code{\link{summary.fixest}} to see the results with the appropriate standard-errors,
\code{\link{fixef.fixest}} to extract the fixed-effects coefficients, and the function \code{\link{etable}}
to visualize the results of multiple estimations.
}
\author{
Laurent Berge
}
