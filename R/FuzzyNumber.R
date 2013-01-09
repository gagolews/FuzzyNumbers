## This file is part of the FuzzyNumbers library.
##
## Copyright 2012 Marek Gagolewski
##
##
## FuzzyNumbers is free software: you can redistribute it and/or modify
## it under the terms of the GNU Lesser General Public License as published by
## the Free Software Foundation, either version 3 of the License, or
## (at your option) any later version.
##
## FuzzyNumbers is distributed in the hope that it will be useful,
## but WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.See the
## GNU Lesser General Public License for more details.
##
## You should have received a copy of the GNU Lesser General Public License
## along with FuzzyNumbers. If not, see <http://www.gnu.org/licenses/>.


#' S4 class representing a fuzzy number
#'
#' Formally, a fuzzy number \eqn{A} (Dubois, Prade, 1978) is a fuzzy subset of the
#' real line \eqn{R}  with membership function \eqn{\mu} given by:
#' \tabular{lll}{
#'                    \tab | \eqn{0}                      \tab if \eqn{x    < a1}, \cr
#'                    \tab | \eqn{left((x-a1)/(a2-a1))}   \tab if \eqn{a1 \le x  < a2}, \cr
#' \eqn{\mu(x)}     = \tab | \eqn{1}                      \tab if \eqn{a2 \le x  \le a3}, \cr
#'                    \tab | \eqn{right((x-a3)/(a4-a3))}  \tab if \eqn{a3   < x  \le a4}, \cr
#'                    \tab | \eqn{0}                      \tab if \eqn{a4   < x}, \cr
#' }
#' where \eqn{a1,a2,a3,a4\in R}, \eqn{a1 \le a2 \le a3 \le a4},
#' \eqn{left: [0,1]\to[0,1]}{left: [0,1]->[0,1]} is a nondecreasing function
#' called the \emph{left side generator of \eqn{A}},
#' and \eqn{right: [0,1]\to[0,1]}{right: [0,1]->[1,0]} is a nonincreasing function
#' called the \emph{right side generator of \eqn{A}}.
#' \cr
#' Alternatively, it may be shown that each fuzzy number \eqn{A} may be uniquely determined
#' by specifying its \eqn{\alpha}-cuts, \eqn{A(\alpha)}. We have \eqn{A(0)=[a1,a4]} and
#' \deqn{A(\alpha)=[a1+(a2-a1)*lower(\alpha), a3+(a4-a3)*upper(\alpha)]}
#' for \eqn{0<\alpha\le 1}, where \eqn{lower: [0,1]\to[0,1]}{lower: [0,1]->[0,1]}
#' and \eqn{upper: [0,1]\to[0,1]}{upper: [0,1]->[1,0]}
#' are, respectively, strictly increasing and decreasing functions
#' satisfying \eqn{lower(\alpha)=\inf\{x: \mu(x)\ge\alpha\}}{lower(\alpha)=inf(x: \mu(x)\ge\alpha)}
#' and  \eqn{upper(\alpha)=\sup\{x: \mu(x)\ge\alpha\}}{upper(\alpha)=sup(x: \mu(x)\ge\alpha)}.
#' \cr
#' Please note that many algorithms that deal with fuzzy numbers often use
#' \eqn{\alpha}-cuts rather than side functions.
#' 
#' Note that the \pkg{FuzzyNumbers} package also deals with particular types
#' of fuzzy numbers like trapezoidal, piecewise linear, or ``parametric'' FNs
#' (see \code{\link{TrapezoidalFuzzyNumber-class}}
#' \code{\link{PiecewiseLinearFuzzyNumber-class}},
#' \code{\link{PowerFuzzyNumber-class}},
#' \code{\link{DiscontinuousFuzzyNumber-class}})
#' 
#' @section Slots:
#'  \describe{
#'    \item{\code{a1}:}{Single numeric value specifying the left bound for the support.}
#'    \item{\code{a2}:}{Single numeric value specifying the left bound for the core.}
#'    \item{\code{a3}:}{Single numeric value specifying the right bound for the core.}
#'    \item{\code{a4}:}{Single numeric value specifying the right bound for the support.}
#'    \item{\code{lower}:}{A nondecreasing function [0,1]->[0,1] that gives the lower alpha-cut bound.}
#'    \item{\code{upper}:}{A nonincreasing function [0,1]->[1,0] that gives the upper alpha-cut bound.}
#'    \item{\code{left}:}{A nondecreasing function [0,1]->[0,1] that gives the left side function.}
#'    \item{\code{right}:}{A nonincreasing function [0,1]->[1,0] that gives the right side function.}
#'  }
#' 
#' @exportClass FuzzyNumber
#' @name FuzzyNumber-class
#' @seealso \code{\link{FuzzyNumber}} for a convenient constructor,
#' \code{\link{convert.side}} for creating side functions generators,
#' \code{\link{convert.alpha}} for creating alpha-cut bounds generators,
#' \code{\link{approx.invert}} for inverting side functions/alpha-cuts numerically
#' @docType class
#' @family FuzzyNumber-method
#' @examples
#' showClass("FuzzyNumber")
#' showMethods(classes="FuzzyNumber")
setClass(
   Class="FuzzyNumber",
   representation(
      a1="numeric",
      a2="numeric",
      a3="numeric",
      a4="numeric",
      lower="function",
      upper="function",
      left="function",
      right="function"
   ),
   prototype=prototype(
      left=function(x)      rep(NA_real_, length(x)),
      right=function(x)     rep(NA_real_, length(x)),
      lower=function(alpha) rep(NA_real_, length(alpha)),
      upper=function(alpha) rep(NA_real_, length(alpha))
   ),
   validity=function(object)
   {
      if (length(object@a1) != 1 || length(object@a2) != 1 ||
          length(object@a3) != 1 || length(object@a4) != 1 ||
          any(!is.finite(c(object@a1, object@a2, object@a3, object@a4))))
         return("Each of `a1', `a2', `a3', and `a4' should be a single finite real number")

      if (is.unsorted(c(object@a1, object@a2, object@a3, object@a4)))
         return("Please provide a1 <= a2 <= a3 <= a4")

      lower01 <- object@lower(c(0,1))
      upper01 <- object@upper(c(0,1))
      left01  <- object@left(c(0,1))
      right01 <- object@right(c(0,1))
      
      if (length(lower01) != 2 || !is.numeric(lower01))
         return("`lower' is not properly vectorized or doesn't give numeric results")
      else if (!is.na(lower01[1])) {
         if (lower01[1] < 0 || lower01[2] > 1 || lower01[1] > lower01[2])
            return("`lower' should be an increasing function [0,1]->[0,1]")
      }
      
      if (length(upper01) != 2 || !is.numeric(upper01))
         return("`upper' is not properly vectorized or doesn't give numeric results")
      else if (!is.na(upper01[1])) {
         if (upper01[2] < 0 || upper01[1] > 1 || upper01[2] > upper01[1])
            return("`upper' should be a decreasing function [0,1]->[1,0]")
      }
      
      if (length(left01) != 2 || !is.numeric(left01))
         return("`left' is not properly vectorized or doesn't give numeric results")
      else if (!is.na(left01[1])) {
         if (left01[1] < 0 || left01[2] > 1 || left01[1] > left01[2])
            return("`left' should be an increasing function [0,1]->[0,1]")
      }
      
      if (length(right01) != 2 || !is.numeric(right01))
         return("`right' is not properly vectorized or doesn't give numeric results")
      else if (!is.na(right01[1])) {
         if (right01[2] < 0 || right01[1] > 1 || right01[2] > right01[1])
            return("`right' should be a decreasing function [0,1]->[1,0]")
      }
      

      if (is.na(right01[1]) != is.na(left01[1]))
         return("Either all or none of `left' and `right' should return NA")

      if (is.na(lower01[1]) != is.na(upper01[1]))
         return("Either all or none of `lower' and `upper' should return NA")

      # OK
      return(TRUE)
   }
)


#' Creates a Fuzzy Number
#'
#' For convenience, objects of class \code{FuzzyNumber} (see \code{\link{FuzzyNumber-class}})
#' may be created with this function.
#'
#' @param a1 a number specyfing left bound of the support
#' @param a2 a number specyfing left bound of the core
#' @param a3 a number specyfing right bound of the core
#' @param a4 a number specyfing right bound of the support
#' @param lower lower alpha-cut bound generator; a nondecreasing function [0,1]->[0,1] or returning NA_real_
#' @param upper upper alpha-cut bound generator; a nonincreasing function [0,1]->[1,0] or returning NA_real_
#' @param left lower side function generator; a nondecreasing function [0,1]->[0,1] or returning NA_real_
#' @param right upper side function generator; a nonincreasing function [0,1]->[1,0] or returning NA_real_
#' @return Object of class \code{FuzzyNumber}
#' @export
FuzzyNumber <- function(a1, a2, a3, a4,
   lower=function(a) rep(NA_real_, length(a)),
   upper=function(a) rep(NA_real_, length(a)),
   left=function(x)  rep(NA_real_, length(x)),
   right=function(x) rep(NA_real_, length(x)))
{
   .Object <- new("FuzzyNumber", a1=a1, a2=a2, a3=a3, a4=a4,
       lower=lower, upper=upper, left=left, right=right)
   .Object
}

#' Coverts a trapezoidal or a piecewise linear fuzzy number object to a (general) FuzzyNumber
#'
#' FuzzyNumber is the base class for all FNs.
#' Note that some functions for TFNs or PLFNs (more specific FNs)
#' work much faster and are more precise. This function shouldn't be
#' used in normal computations.
#' 
#' @param object a trapezoidal or piecewiselinear fuzzy number
#' @return Object of class \code{FuzzyNumber}
#' @export
#' @seealso \code{\link{FuzzyNumber-class}},
#' \code{\link{TrapezoidalFuzzyNumber-class}},
#' \code{\link{PiecewiseLinearFuzzyNumber-class}}
as.FuzzyNumber <- function(object)
{
   if (!inherits(object, "FuzzyNumber"))
      stop("`object' does not inherit from the FuzzyNumber class")

   .Object <- new("FuzzyNumber",
         a1=object@a1, a2=object@a2, a3=object@a3, a4=object@a4,
         left=object@left, right=object@right,
         lower=object@lower, upper=object@upper)
   .Object
}
