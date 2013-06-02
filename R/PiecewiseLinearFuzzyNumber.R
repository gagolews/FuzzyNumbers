## This file is part of the FuzzyNumbers library.
##
## Copyright 2012-2013 Marek Gagolewski
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


#' S4 Class Representing a Piecewise Linear Fuzzy Number
#'
#' A piecewise linear fuzzy number (PLFN) has side functions
#' and alpha-cut bounds that linearly interpolate a given set of points
#' (at fixed alpha-cuts).
#'
#' @section Slots:
#'  \describe{
#'     \item{\code{knot.n}:}{number of knots, a single integer value,
#'     0 for a trapezoidal fuzzy number}
#'     \item{\code{knot.alpha}:}{alpha-cuts, increasingly sorted vector of length \code{knot.n} with elements in [0,1]}
#'     \item{\code{knot.left}:}{nondecreasingly sorted vector of length \code{knot.n}; defines left alpha-cut bounds at knots}
#'     \item{\code{knot.right}:}{nondecreasingly sorted vector of length \code{knot.n}; defines right alpha-cut bounds at knots}
#'  }
#'  
#' @section Extends:
#' Class \code{\linkS4class{FuzzyNumber}}, directly.
#' 
#' @exportClass PiecewiseLinearFuzzyNumber
#' @name PiecewiseLinearFuzzyNumber-class
#' @seealso \code{\link{PiecewiseLinearFuzzyNumber}} for a convenient constructor
#' @family PiecewiseLinearFuzzyNumber-method
#' @docType class
#' @examples
#' showClass("PiecewiseLinearFuzzyNumber")
setClass(
   Class="PiecewiseLinearFuzzyNumber",
   representation(
      knot.n="numeric",
      knot.alpha="numeric",
      knot.left="numeric",
      knot.right="numeric"
   ),
   prototype=prototype(
      lower=function(a) rep(NA_real_, length(a)),
      upper=function(a) rep(NA_real_, length(a)),
      left=function(x)  rep(NA_real_, length(x)),
      right=function(x) rep(NA_real_, length(x))
   ),
   validity=function(object)
   {
      if (length(object@knot.n) != 1) return("`knot.n' should be a vector of length 1")
      object@knot.n <- as.integer(object@knot.n)
      if (!is.finite(object@knot.n)) return("`knot.n' should be finite")
      
      if (object@knot.n < 0) return("`knot.n' should be >= 0")
      if (object@knot.n != length(object@knot.alpha)) return("length of `knot.alpha' should be equal to `knot.n'")
      if (object@knot.n != length(object@knot.left))  return("length of `knot.left' should be equal to `knot.n'")
      if (object@knot.n != length(object@knot.right)) return("length of `knot.right' should be equal to `knot.n'")
      
      if (object@knot.n > 0)
      {
         if (is.unsorted(object@knot.left))  return("`knot.left' should be sorted nondecreasingly")
         if (is.unsorted(object@knot.right)) return("`knot.right' should be sorted nondecreasingly")
         
         if (!is.finite(object@knot.left)  || any(object@knot.left < object@a1 | object@knot.left > object@a2))
            return("`knot.left' should be a vector with elements in [a1,a2]")
         if (!is.finite(object@knot.right) || any(object@knot.right < object@a3 | object@knot.left > object@a4))
            return("`knot.right' should be a vector with elements in [a3,a4]")
         
         if (any(diff(object@knot.alpha) <= 0)) return("`knot.alpha' should be sorted nondecreasingly and be unique");
         if (!is.finite(object@knot.alpha) || any(object@knot.alpha < 0 | object@knot.alpha > 1))
            return("`knot.alpha' should be a vector with elements in [0,1]")
      }
      
      return(TRUE)
   },
   contains="FuzzyNumber"
)




setMethod(
   f="initialize",
   signature("PiecewiseLinearFuzzyNumber"),
   definition=function(.Object, ...)
   {
      .Object <- callNextMethod()
      
      kl <- c(0,(.Object@knot.left -.Object@a1)/(.Object@a2-.Object@a1),1)
      kr <- c(0,(.Object@knot.right-.Object@a3)/(.Object@a4-.Object@a3),1)
      
      al <- c(0,.Object@knot.alpha,1)
      ar <- c(1,rev(.Object@knot.alpha),0)
      
      # be careful for equal knot positions! (ties="ordered" solves that)
      .Object@left  <- approxfun(kl, al, method="linear",
                                 yleft=NA, yright=NA, ties="ordered")
      .Object@right <- approxfun(kr, ar, method="linear",
                                 yleft=NA, yright=NA, ties="ordered")
      
      # no ties to specify - knot.alpha is unique
      .Object@lower <- approxfun(al, kl, method="linear",
                                 yleft=NA, yright=NA)
      .Object@upper <- approxfun(ar, kr, method="linear",
                                 yleft=NA, yright=NA)
      
      return(.Object)
   }
)


#' Creates a Piecewise Linear Fuzzy Number
#'
#' For convenience, objects of class \code{\linkS4class{PiecewiseLinearFuzzyNumber}}
#' may be created with this function.
#' 
#' If \code{a1}, \code{a2}, \code{a3}, and \code{a4} are missing,
#' then \code{knot.left} and \code{knot.right} may be of length \code{knot.n+2}.
#' 
#' If \code{knot.n} is not given, then it guessed from \code{length(knot.left)}.
#' If \code{knot.alpha} is missing, then the knots will be equally distributed
#' on the interval [0,1].
#'
#' @param a1 a number specyfing left bound of the support
#' @param a2 a number specyfing left bound of the core
#' @param a3 a number specyfing right bound of the core
#' @param a4 a number specyfing right bound of the support
#' @param knot.n the number of knots
#' @param knot.alpha \code{knot.n} alpha-cut values at knots
#' @param knot.left \code{knot.n} knots on the left side; a nondecreasingly sorted vector with elements in [\code{a1},\code{a2}]
#' @param knot.right \code{knot.n} knots on the right side; a nondecreasingly sorted vector with elements in [\code{a3},\code{a4}]
#' @return An object of class \code{\linkS4class{PiecewiseLinearFuzzyNumber}}.
#' @export
#' 
#' @family PiecewiseLinearFuzzyNumber-method
PiecewiseLinearFuzzyNumber <- function(a1, a2, a3, a4,
                                       knot.n=0, knot.alpha=numeric(0),
                                       knot.left=numeric(0), knot.right=numeric(0))
{
   stopifnot(length(knot.left) == length(knot.right))
   
   if (missing(a1) && missing(a2) && missing(a3) && missing(a4)) {
      if (missing(knot.n))
         knot.n <- length(knot.left)-2
      
      if (missing(knot.alpha))
         knot.alpha <- seq(0, 1, length.out=knot.n+2)[-c(1,knot.n+2)]
      
      new("PiecewiseLinearFuzzyNumber", a1=knot.left[1], a2=knot.left[knot.n+2], a3=knot.right[1], a4=knot.right[knot.n+2],
          knot.n=knot.n, knot.alpha=knot.alpha, knot.left=knot.left[-c(1,knot.n+2)], knot.right=knot.right[-c(1,knot.n+2)])
   }
   else {
      if (missing(knot.n))
         knot.n <- length(knot.left)
      
      if (missing(knot.alpha))
         knot.alpha <- seq(0, 1, length.out=knot.n+2)[-c(1,knot.n+2)]
      
      new("PiecewiseLinearFuzzyNumber", a1=a1, a2=a2, a3=a3, a4=a4,
          knot.n=knot.n, knot.alpha=knot.alpha, knot.left=knot.left, knot.right=knot.right)
   }
}







#' Coverts an Object to a Piecewise Linear Fuzzy Number
#' 
#' This method is only for exact conversion.
#' For other cases (e.g. general FNs), use
#' \code{\link{piecewiseLinearApproximation}}.
#' 
#' @section Methods:
#' \describe{
#'      \item{\code{signature(object = "TrapezoidalFuzzyNumber")}}{}
#'      \item{\code{signature(object = "numeric")}}{}
#'      \item{\code{signature(object = "PiecewiseLinearFuzzyNumber")}}{
#'      in cases when conversion will not be exact,
#'      gives error and suggests using \code{\link{piecewiseLinearApproximation}}}
#'      \item{\code{signature(object = "FuzzyNumber")}}{gives error and suggests using \code{\link{piecewiseLinearApproximation}}}
#' }
#'
#' @param object a trapezoidal fuzzy number
#' @param knot.n the number of knots
#' @param knot.alpha \code{knot.n} alpha-cut values at knots,
#' defaults to uniformly distributed knots
#' @return Object of class \code{\linkS4class{PiecewiseLinearFuzzyNumber}}
#' 
#' 
#' @name as.PiecewiseLinearFuzzyNumber
#' @docType methods
#' @rdname as.PiecewiseLinearFuzzyNumber
#' @family TrapezoidalFuzzyNumber-method
#' @family PiecewiseLinearFuzzyNumber-method
#' @family FuzzyNumber-method
#' @aliases as.PiecewiseLinearFuzzyNumber,TrapezoidalFuzzyNumber-method
#'          as.PiecewiseLinearFuzzyNumber,numeric-method
#'          as.PiecewiseLinearFuzzyNumber,FuzzyNumber-method
#'          as.PiecewiseLinearFuzzyNumber,TrapezoidalFuzzyNumber-method
#' @exportMethod as.PiecewiseLinearFuzzyNumber
setGeneric("as.PiecewiseLinearFuzzyNumber",
           function(object, ...) standardGeneric("as.PiecewiseLinearFuzzyNumber"))




setMethod(
   f="as.PiecewiseLinearFuzzyNumber",
   signature(object="TrapezoidalFuzzyNumber"),
   definition=function(object, knot.n=0, knot.alpha=seq(0, 1, length.out=knot.n+2)[-c(1,knot.n+2)])
   {
      stopifnot(length(knot.n) == 1, knot.n >= 0)
      stopifnot(knot.n == length(knot.alpha)) 
      
      a <- alphacut(object, c(0, knot.alpha, 1))
      PiecewiseLinearFuzzyNumber(knot.left=a[,1], knot.right=rev(a[,2]))
   })



setMethod(
   f="as.PiecewiseLinearFuzzyNumber",
   signature(object="numeric"),
   definition=function(object, knot.n=0, knot.alpha=seq(0, 1, length.out=knot.n+2)[-c(1,knot.n+2)])
   {
      stopifnot(length(object) == 1, is.finite(object))
      stopifnot(length(knot.n) == 1, knot.n >= 0)
      stopifnot(knot.n == length(knot.alpha)) 
      
      PiecewiseLinearFuzzyNumber(a1=object, a2=object, a3=object, a4=object,
                                 knot.n=knot.n, knot.alpha=knot.alpha, 
                                 knot.left=rep(object, knot.n), 
                                 knot.right=rep(object, knot.n))
   })



setMethod(
   f="as.PiecewiseLinearFuzzyNumber",
   signature(object="FuzzyNumber"),
   definition=function(object, knot.n=0, knot.alpha=seq(0, 1, length.out=knot.n+2)[-c(1,knot.n+2)])
   {
      stop("as.PiecewiseLinearFuzzyNumber() is only for exact conversion. Use piecewiseLinearApproximation() instead.")
   })



setMethod(
   f="as.PiecewiseLinearFuzzyNumber",
   signature(object="PiecewiseLinearFuzzyNumber"),
   definition=function(object, knot.n=0, knot.alpha=seq(0, 1, length.out=knot.n+2)[-c(1,knot.n+2)])
   {
      stopifnot(length(knot.n) == 1, knot.n >= 0)
      stopifnot(knot.n == length(knot.alpha))   
      
      if (length(setdiff(object@knot.alpha, knot.alpha)) != 0)
         stop("as.PiecewiseLinearFuzzyNumber() is only for exact conversion. Use piecewiseLinearApproximation() instead.")
      
      a <- alphacut(object, c(0, knot.alpha, 1))
      PiecewiseLinearFuzzyNumber(knot.left=a[,1], knot.right=rev(a[,2]))
   })
