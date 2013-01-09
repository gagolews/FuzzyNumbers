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




setGeneric("width",
           function(object, ...) standardGeneric("width"))



#' Calculate the width of a fuzzy number
#'
#' The width is a measure of nonspecificity of a fuzzy number.
#' 
#' The width of \eqn{A} is defined as
#' \eqn{width(A) := EI_U(A) - EI_L(A)},
#' where \eqn{EI} is the \code{expectedInterval}.
#' 
#' @section Methods:
#' \describe{
#'      \item{\code{signature(object = "FuzzyNumber")}}{
#'      
#'   }
#' }
#' @exportMethod width
#' @name width
#' @aliases width,FuzzyNumber-method
#' @rdname width-methods
#' @seealso \code{\link{expectedInterval}} on which this function is based
#' @docType methods
#' @family FuzzyNumber-method
#' @references
#' Chanas S. (2001), On the interval approximation of a fuzzy number,
#' Fuzzy Sets and Systems 122, pp. 353-356.\cr
setMethod(
   f="width",
   signature(object="FuzzyNumber"),
   definition=function(object, ...)
   {
      return(diff(expectedInterval(object, ...)))
   }
)

