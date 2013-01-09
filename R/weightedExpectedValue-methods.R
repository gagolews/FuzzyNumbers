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



setGeneric("weightedExpectedValue",
           function(object, w, ...) standardGeneric("weightedExpectedValue"))



#' Calculate the weighted expected value of a fuzzy number
#'
#' The weighted (\eqn{0<w<1})) expected value of \eqn{A} is defined as
#' \eqn{EV_w(A) := (1-w) EI_U(A) - w EI_L(A)},
#' where \eqn{EI} is the \code{expectedInterval}.
#' 
#' @section Methods:
#' \describe{
#'      \item{\code{signature(object = "FuzzyNumber", w = "numeric")}}{
#'      
#'   }
#' }
#' @exportMethod weightedExpectedValue
#' @name weightedExpectedValue
#' @aliases weightedExpectedValue,FuzzyNumber,numeric-method
#' @rdname weightedExpectedValue-methods
#' @seealso \code{\link{expectedInterval}} on which this function is based
#' @family FuzzyNumber-method
#' @docType methods
setMethod(
   f="weightedExpectedValue",
   signature(object="FuzzyNumber", w="numeric"),
   definition=function(object, w, ...)
   {
      EI <- expectedInterval(object, ...)
      return((1-w)*EI[1] + w*EI[2])
   }
)

