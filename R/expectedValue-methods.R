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


setGeneric("expectedValue",
           function(object, ...) standardGeneric("expectedValue"))



#' Calculate the expected value of a fuzzy number (defuzzify)
#'
#' The expected value of \eqn{A} is defined as
#' \eqn{EV_w(A) := EI_U(A) - EI_L(A)},
#' where \eqn{EI} is the \code{expectedInterval}.
#' 
#' @section Methods:
#' \describe{
#'      \item{\code{signature(object = "FuzzyNumber")}}{ }
#' }
#' @exportMethod expectedValue
#' @name expectedValue
#' @aliases expectedValue,FuzzyNumber-method
#' @rdname expectedValue-methods
#' @docType methods
#' @family FuzzyNumber-method
#' @seealso \code{\link{expectedInterval}} on which this function is based,
#' and also \code{\link{weightedExpectedValue}}
setMethod(
   f="expectedValue",
   signature(object="FuzzyNumber"),
   definition=function(object, ...)
   {
      return(mean(expectedInterval(object, ...)))
   }
)
