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



#' S4 class representing a fuzzy number with sides given by power functions
#'
#'
#' @section Slots:
#'  \describe{
#'    \item{\code{p.left}:}{Object of class \code{"numeric"}; 1.0 to get a trapezoidal FN }
#'    \item{\code{p.right}:}{Object of class \code{"numeric"}; 1.0 to get a trapezoidal FN }
#'  }
#'
#' @section Extends:
#' Class \code{\linkS4class{FuzzyNumber}}, directly. 
#'
#' @exportClass PowerFuzzyNumber
#' @name PowerFuzzyNumber-class
#' @seealso \code{\link{PowerFuzzyNumber}} for a convenient constructor
#' @docType class
#' @examples
#' showClass("PowerFuzzyNumber")
setClass(
   Class="PowerFuzzyNumber",
   representation(
      p.left="numeric",
      p.right="numeric"
   ),
   prototype=prototype(
      p.left=1.0,
      p.right=1.0
   ),
   validity=function(object)
   {
      if (object@p.left <= 0) return("`p.left' should be > 0")
      if (object@p.right <= 0) return("`p.right' should be > 0")

      return(TRUE);
   },
   contains="FuzzyNumber"
)



setMethod(
   f="initialize",
   signature("PowerFuzzyNumber"),
   definition=function(.Object, ...)
   {
      .Object <- callNextMethod()
      
      e <- new.env();
      
      e$p.left  <- p.left  <- .Object@p.left    # p.left <- ... to avoid
      e$p.right <- p.right <- .Object@p.right   # PKG CHECK problems

      .Object@left   <- function(x)             x^(p.left)
      .Object@right  <- function(x)         (1-x)^(p.right)
      .Object@lower  <- function(alpha)     alpha^(1.0/p.left)
      .Object@upper  <- function(alpha)   1-alpha^(1.0/p.right)

      environment(.Object@left)  <- e
      environment(.Object@right) <- e
      environment(.Object@lower) <- e
      environment(.Object@upper) <- e

      return(.Object)
   }
)


#' Creates a ``parametric'' fuzzy number with sides given by power functions
#'
#' For convenience, objects of class \code{\linkS4class{PowerFuzzyNumber}}
#' may be created with this function.
#'
#' @param a1 a number specyfing left bound of the support
#' @param a2 a number specyfing left bound of the core
#' @param a3 a number specyfing right bound of the core
#' @param a4 a number specyfing right bound of the support
#' @param p.left a positive number specyfing the exponent for the left side
#' @param p.right a positive number specyfing the exponent for the right side
#' @return Object of class \code{\linkS4class{PowerFuzzyNumber}}
#' @export
PowerFuzzyNumber <- function(a1, a2, a3, a4, p.left=1.0, p.right=1.0)
{
   .Object <- new("PowerFuzzyNumber", a1=a1, a2=a2, a3=a3, a4=a4,
                                      p.left=p.left, p.right=p.right)
   .Object
}


