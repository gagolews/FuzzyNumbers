% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/methods-supp.R
\docType{methods}
\name{supp}
\alias{supp}
\alias{supp,FuzzyNumber-method}
\title{Calculate the Support of a Fuzzy Number}
\usage{
\S4method{supp}{FuzzyNumber}(object)
}
\arguments{
\item{object}{a fuzzy number}
}
\value{
Returns a numeric vector of length 2.
}
\description{
We have \eqn{\mathrm{supp}(A) := [a1,a4]}{supp(A) := [a1,a4]}.
This gives the values that a fuzzy number possibly may represent.
}
\seealso{
Other FuzzyNumber-method: 
\code{\link{Arithmetic}},
\code{\link{Extract}},
\code{\link{FuzzyNumber-class}},
\code{\link{FuzzyNumber}},
\code{\link{alphaInterval}()},
\code{\link{alphacut}()},
\code{\link{ambiguity}()},
\code{\link{as.FuzzyNumber}()},
\code{\link{as.PiecewiseLinearFuzzyNumber}()},
\code{\link{as.PowerFuzzyNumber}()},
\code{\link{as.TrapezoidalFuzzyNumber}()},
\code{\link{as.character}()},
\code{\link{core}()},
\code{\link{distance}()},
\code{\link{evaluate}()},
\code{\link{expectedInterval}()},
\code{\link{expectedValue}()},
\code{\link{integrateAlpha}()},
\code{\link{piecewiseLinearApproximation}()},
\code{\link{plot}()},
\code{\link{show}()},
\code{\link{trapezoidalApproximation}()},
\code{\link{value}()},
\code{\link{weightedExpectedValue}()},
\code{\link{width}()}

Other alpha_cuts: 
\code{\link{alphacut}()},
\code{\link{core}()}
}
\concept{FuzzyNumber-method}
\concept{alpha_cuts}
