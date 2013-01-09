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




setGeneric("value",
           function(object, ...) standardGeneric("value"))


#' Calculate the value of a fuzzy number (defuzzify)
#'
#' The value of \eqn{A} (Delgrado et al, 1998) is defined as
#' \eqn{val(A) := \int_0^1 \alpha\left(A_L(\alpha)+A_U(\alpha)\right)\,d\alpha}{val(A) := int_0^1 \alpha(A_L(\alpha) + A_U(\alpha))d\alpha}.
#' 
#' @section Methods:
#' \describe{
#'      \item{\code{signature(object = "FuzzyNumber")}}{ }
#' }
#' @exportMethod value
#' @name value
#' @aliases value,FuzzyNumber-method
#' @rdname value-methods
#' @docType methods
#' @seealso \code{\link{alphaInterval}} on which this function is based
#' @family FuzzyNumber-method
#' @references
#' Delgado M., Vila M.A., Voxman W. (1998), On a canonical representation of a fuzzy number,
#' Fuzzy Sets and Systems 93, pp. 125-135.\cr
setMethod(
   f="value",
   signature(object="FuzzyNumber"),
   definition=function(object, ...)
   {
      return(sum(alphaInterval(object, ...)))
   }
)


