% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fixest_multi.R
\name{summary.fixest_multi}
\alias{summary.fixest_multi}
\title{Summary for fixest_multi objects}
\usage{
\method{summary}{fixest_multi}(
  object,
  type = "short",
  se = NULL,
  cluster = NULL,
  dof = NULL,
  .vcov,
  stage = 2,
  lean = FALSE,
  n,
  ...
)
}
\arguments{
\item{object}{A \code{fixest_multi} object, obtained from a \code{fixest} estimation leading to multiple results.}

\item{type}{A character either equal to \code{"short"}, \code{"long"}, \code{"compact"}, or \code{"se_compact"}. If \code{short}, only the table of coefficients is displayed for each estimation. If \code{long}, then the full results are displayed for each estimation. If \code{compact}, a \code{data.frame} is returned with one line per model and the formatted coefficients + standard-errors in the columns. If \code{se_compact}, a \code{data.frame} is returned with one line per model, one numeric column for each coefficient and one numeric column for each standard-error.}

\item{se}{Character scalar. Which kind of standard error should be computed: \dQuote{standard}, \dQuote{hetero}, \dQuote{cluster}, \dQuote{twoway}, \dQuote{threeway} or \dQuote{fourway}? By default if there are clusters in the estimation: \code{se = "cluster"}, otherwise \code{se = "standard"}. Note that this argument can be implicitly deduced from the argument \code{cluster}.}

\item{cluster}{Tells how to cluster the standard-errors (if clustering is requested). Can be either a list of vectors, a character vector of variable names, a formula or an integer vector. Assume we want to perform 2-way clustering over \code{var1} and \code{var2} contained in the data.frame \code{base} used for the estimation. All the following \code{cluster} arguments are valid and do the same thing: \code{cluster = base[, c("var1", "var2")]}, \code{cluster = c("var1", "var2")}, \code{cluster = ~var1+var2}. If the two variables were used as clusters in the estimation, you could further use \code{cluster = 1:2} or leave it blank with \code{se = "twoway"} (assuming \code{var1} [resp. \code{var2}] was the 1st [res. 2nd] cluster). You can interact two variables using \code{^} with the following syntax: \code{cluster = ~var1^var2} or \code{cluster = "var1^var2"}.}

\item{dof}{An object of class \code{dof.type} obtained with the function \code{\link[fixest]{dof}}. Represents how the degree of freedom correction should be done.You must use the function \code{\link[fixest]{dof}} for this argument. The arguments and defaults of the function \code{\link[fixest]{dof}} are: \code{adj = TRUE}, \code{fixef.K="nested"}, \code{cluster.adj = TRUE}, \code{cluster.df = "conventional"}, \code{t.df = "conventional"}, \code{fixef.force_exact=FALSE)}. See the help of the function \code{\link[fixest]{dof}} for details.}

\item{.vcov}{A user provided covariance matrix or a function computing this matrix. If a matrix, it must be a square matrix of the same number of rows as the number of variables estimated. If a function, it must return the previously mentioned matrix.}

\item{stage}{Can be equal to \code{2} (default), \code{1}, \code{1:2} or \code{2:1}. Only used if the object is an IV estimation: defines the stage to which \code{summary} should be applied. If \code{stage = 1} and there are multiple endogenous regressors or if \code{stage} is of length 2, then an object of class \code{fixest_multi} is returned.}

\item{lean}{Logical, default is \code{FALSE}. Used to reduce the (memory) size of the summary object. If \code{TRUE}, then all objects of length N (the number of observations) are removed from the result. Note that some \code{fixest} methods may consequently not work when applied to the summary.}

\item{n}{Integer, default is missing (means Inf). Number of coefficients to display when the print method is used.}

\item{...}{Not currently used.}
}
\value{
It returns either an object of class \code{fixest_multi} (if \code{type} equals \code{short} or \code{long}), either a \code{data.frame} (if type equals \code{compact} or \code{se_compact}).
}
\description{
Summary information for fixest_multi objects. In particular, this is used to specify the type of standard-errors to be computed.
}
\examples{

base = iris
names(base) = c("y", "x1", "x2", "x3", "species")

# Multiple estimation
res = feols(y ~ csw(x1, x2, x3), base, split = ~species)

# By default, the type is "short"
# You can stil use the arguments from summary.fixest
summary(res, cluster = ~ species)

summary(res, type = "long")

summary(res, type = "compact")

summary(res, type = "se_compact")


}
\seealso{
The main fixest estimation functions: \code{\link[fixest]{feols}}, \code{\link[fixest:feglm]{fepois}}, \code{\link[fixest:femlm]{fenegbin}}, \code{\link[fixest]{feglm}}, \code{\link[fixest]{feNmlm}}. Tools for mutliple fixest estimations: \code{\link[fixest]{summary.fixest_multi}}, \code{\link[fixest]{print.fixest_multi}}, \code{\link[fixest]{as.list.fixest_multi}}, \code{\link[fixest]{sub-sub-.fixest_multi}}, \code{\link[fixest]{sub-.fixest_multi}}, \code{\link[fixest]{cash-.fixest_multi}}.
}