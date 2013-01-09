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


#' S4 class representing a piecewise linear fuzzy number
#'
#' A piecewise linear fuzzy number (PLFN) has side functions
#' and alpha-cut bounds that linearly interpolate a given set of points
#' (at fixed alpha-cuts).
#'
#' @section Slots:
#'  \describe{
#'     \item{\code{knot.n}:}{Object of class \code{"numeric"} ~~ }
#'     \item{\code{knot.alpha}:}{Object of class \code{"numeric"} ~~ }
#'     \item{\code{knot.left}:}{Object of class \code{"numeric"} ~~ }
#'     \item{\code{knot.right}:}{Object of class \code{"numeric"} ~~ }
#'  }
#'  
#' @section Extends:
#' Class \code{\linkS4class{FuzzyNumber}}, directly.
#' 
#' @exportClass PiecewiseLinearFuzzyNumber
#' @name PiecewiseLinearFuzzyNumber-class
#' @seealso \code{\link{PiecewiseLinearFuzzyNumber}} for a convenient constructor
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


#' Creates a piecewise linear fuzzy number
#'
#' For convenience, objects of class \code{\linkS4class{PiecewiseLinearFuzzyNumber}}
#' may be created with this function.
#'
#' @param a1 a number specyfing left bound of the support
#' @param a2 a number specyfing left bound of the core
#' @param a3 a number specyfing right bound of the core
#' @param a4 a number specyfing right bound of the support
#' @param knot.n the number of knots
#' @param knot.alpha \code{knot.n} alpha-cut values at knots
#' @param knot.left \code{knot.n} knots on the left side; a nondecreasingly sorted vector with elements in [\code{a1},\code{a2}]
#' @param knot.right \code{knot.n} knots on the right side; a nondecreasingly sorted vector with elements in [\code{a3},\code{a4}]
#' @return Object of class \code{\linkS4class{PiecewiseLinearFuzzyNumber}}
#' @export
PiecewiseLinearFuzzyNumber <- function(a1, a2, a3, a4,
   knot.n=0, knot.alpha=numeric(0),
   knot.left=numeric(0), knot.right=numeric(0))
{
   .Object <- new("PiecewiseLinearFuzzyNumber", a1=a1, a2=a2, a3=a3, a4=a4,
         knot.n=knot.n, knot.alpha=knot.alpha, knot.left=knot.left, knot.right=knot.right)
   .Object
}




#' Coverts a trapezoidal fuzzy number object to a piecewise linear fuzzy number
#'
#' @param object a trapezoidal fuzzy number
#' @param knot.n the number of knots
#' @param knot.alpha \code{knot.n} alpha-cut values at knots
#' @return Object of class \code{\linkS4class{PiecewiseLinearFuzzyNumber}}
#' @export
as.PiecewiseLinearFuzzyNumber <- function(object, knot.n=0, knot.alpha=numeric(0))
{
   if (class(object) != "TrapezoidalFuzzyNumber") stop("`object' is not an instance of the TrapezoidalFuzzyNumber class")

   left  <- approxfun(c(0,1), c(object@a1,object@a2), method="linear")  # no ties to specify - knot.alpha is unique
   right <- approxfun(c(1,0), c(object@a3,object@a4), method="linear")  # no ties to specify - knot.alpha is unique

   if (knot.n < 0) stop("`knot.n' should be >= 0")
   if (knot.n == 0) return(new("PiecewiseLinearFuzzyNumber", a1=object@a1, a2=object@a2, a3=object@a3, a4=object@a4))

   if (knot.n != length(knot.alpha)) stop("length of `knot.alpha' should be equal to `knot.n'")

   .Object <- new("PiecewiseLinearFuzzyNumber", a1=object@a1, a2=object@a2, a3=object@a3, a4=object@a4,
         knot.n=knot.n, knot.alpha=knot.alpha, knot.left=left(knot.alpha), knot.right=right(rev(knot.alpha)))
   .Object
}
