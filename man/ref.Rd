% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/MiscFuns.R
\name{ref}
\alias{ref}
\title{Refactors a variable}
\usage{
ref(x, ref)
}
\arguments{
\item{x}{A vector of any type (must be atomic though).}

\item{ref}{A vector or a list, or special binning values (explained later). If a vector, it must correspond to (partially matched) values of the vector \code{x}. The vector \code{x} which will be transformed into a factor and these values will be placed first in the levels. That's the main usage of this function. You can also bin on-the-fly the values of \code{x}, using the same syntax as the function \code{\link[fixest]{bin}}. Here's a description of what bin does: To create a new value from old values, use \code{bin = list("new_value"=old_values)} with \code{old_values} a vector of existing values. You can use \code{.()} for \code{list()}.
It accepts regular expressions, but they must start with an \code{"@"}, like in \code{bin="@Aug|Dec"}. It accepts one-sided formulas which must contain the variable \code{x}, e.g. \code{bin=list("<2" = ~x < 2)}.
The names of the list are the new names. If the new name is missing, the first value matched becomes the new name. In the name, adding \code{"@d"}, with \code{d} a digit, will relocate the value in position \code{d}: useful to change the position of factors.
If the vector \code{x} is numeric, you can use the special value \code{"bin::digit"} to group every \code{digit} element.
For example if \code{x} represents years, using \code{bin="bin::2"} creates bins of two years.
With any data, using \code{"!bin::digit"} groups every digit consecutive values starting from the first value.
Using \code{"!!bin::digit"} is the same but starting from the last value.
With numeric vectors you can: a) use \code{"cut::n"} to cut the vector into \code{n} equal parts, b) use \code{"cut::a]b["} to create the following bins: \code{[min, a]}, \code{]a, b[}, \code{[b, max]}.
The latter syntax is a sequence of number/quartile (q0 to q4)/percentile (p0 to p100) followed by an open or closed square bracket. You can add custom bin names by adding them in the character vector after \code{'cut::values'}. See details and examples. Dot square bracket expansion (see \code{\link[fixest]{dsb}}) is enabled.}
}
\value{
It returns a factor of the same length as \code{x}, where levels have been modified according to the argument \code{ref}.
}
\description{
Takes a variables of any types, transforms it into a factors, and modifies the values of the factors. Useful in estimations when you want to set some value of a vector as a reference.
}
\section{\code{bin} vs \code{ref}}{


The functions \code{\link[fixest]{bin}} and \code{\link[fixest]{ref}} are able to do the same thing, then why use one instead of the other? Here are the differences:

\itemize{
\item{}{\code{ref} always returns a factor. This is in contrast with \code{bin} which returns, when possible, a vector of the same type as the vector in input.}
\item{}{\code{ref} always places the values modified in the first place of the factor levels. On the other hand, \code{bin} tries to not modify the ordering of the levels. It is possible to make \code{bin} mimic the behavior of \code{ref} by adding an \code{"@"} as the first element of the list in the argument \code{bin}.}
 \item{}{when a vector (and not a list) is given in input, \code{ref} will place each element of the vector in the first place of the factor levels. The behavior of \code{bin} is totally different, \code{bin} will transform all the values in the vector into a single value in \code{x} (i.e. it's binning).}
}
}

\examples{

data(airquality)

# A vector of months
month_num = airquality$Month
month_lab = c("may", "june", "july", "august", "september")
month_fact = factor(month_num, labels = month_lab)
table(month_num)
table(month_fact)

#
# Main use
#

# Without argument: equivalent to as.factor
ref(month_num)

# Main usage: to set a level first:
# (Note that partial matching is enabled.)
table(ref(month_fact, "aug"))

# You can rename the level on-the-fly
# (Northern hemisphere specific!)
table(ref(month_fact, .("Hot month"="aug",
                        "Late summer" = "sept")))


# Main use is in estimations:
a = feols(Petal.Width ~ Petal.Length + Species, iris)

# We change the reference
b = feols(Petal.Width ~ Petal.Length + ref(Species, "vers"), iris)

etable(a, b)


#
# Binning
#

# You can also bin factor values on the fly
# Using @ first means a regular expression will be used to match the values.
# Note that the value created is placed first.
# To avoid that behavior => use the function "bin"
table(ref(month_fact, .(summer = "@jul|aug|sep")))

# Please refer to the example in the bin help page for more example.
# The syntax is the same.


#
# Precise relocation
#

# You can place a factor at the location you want
#  by adding "@digit" in the name first:
table(ref(month_num, .("@5"=5)))

# Same with renaming
table(ref(month_num, .("@5 five"=5)))


}
\seealso{
To bin the values of a vectors: \code{\link[fixest]{bin}}.
}
\author{
Laurent Berge
}