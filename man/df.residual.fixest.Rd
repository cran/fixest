% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/methods.R
\name{df.residual.fixest}
\alias{df.residual.fixest}
\title{Residual degrees-of-freedom for \code{fixest} objects}
\usage{
\method{df.residual}{fixest}(object, ...)
}
\arguments{
\item{object}{A \code{fixest} estimation, e.g. from \code{\link{feols}} or \code{\link{feglm}}.}

\item{...}{Not currently used}
}
\value{
It returns an integer scalar giving the residuals degrees of freedom of the estimation.
}
\description{
Returns the residual degrees of freedom for a fitted \code{fixest} object
}
\examples{

est = feols(mpg ~ hp, mtcars)
df.residual(est)

}
\seealso{
The function \code{\link{degrees_freedom}} in \code{fixest}.
}
