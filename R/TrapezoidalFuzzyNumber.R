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


#' S4 class representing a trapezoidal fuzzy number
#'
#' 
#'
#' @section Extends:
#' Class \code{\linkS4class{FuzzyNumber}}, directly. 
#'
#' @exportClass TrapezoidalFuzzyNumber
#' @name TrapezoidalFuzzyNumber-class
#' @seealso \code{\link{TrapezoidalFuzzyNumber}} for a convenient constructor
#' @docType class
#' @examples
#' showClass("DiscontinuousFuzzyNumber")
setClass(
   Class="TrapezoidalFuzzyNumber",
   prototype=prototype(
      left=function(x) x,
      right=function(x) 1-x,
      lower=function(alpha) alpha,
      upper=function(alpha) 1-alpha
   ),
   contains="FuzzyNumber"
)


#' Creates a trapezoidal fuzzy number
#'
#' For convenience, objects of class \code{\linkS4class{TrapezoidalFuzzyNumber}}
#' may be created with this function.
#'
#' @param a1 a number specyfing left bound of the support
#' @param a2 a number specyfing left bound of the core
#' @param a3 a number specyfing right bound of the core
#' @param a4 a number specyfing right bound of the support
#' @return Object of class \code{\linkS4class{TrapezoidalFuzzyNumber}}
#' @export
TrapezoidalFuzzyNumber <- function(a1, a2, a3, a4)
{
   .Object <- new("TrapezoidalFuzzyNumber", a1=a1, a2=a2, a3=a3, a4=a4)
   .Object
}

