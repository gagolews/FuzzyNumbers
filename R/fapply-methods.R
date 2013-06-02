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


setGeneric("fapply",
        function(object, fun, ...) standardGeneric("fapply"))




#' Apply a Function on a Fuzzy Number
#' 
#' Applies a given monotonic function
#' using the extension principle (i.e. the function is applied on alpha-cuts).
#' 
#' @section Methods:
#' \describe{
#'      \item{\code{signature(object = "PiecewiseLinearFuzzyNumber", fun = "function")}}{return a PiecewiseLinearFuzzyNumber with the same knot settings}
#' }
#' 
#' 
#' @param object a fuzzy number
#' @param fun a monotonic, vectorized R function
#' @param ... additional arguments passed to \code{fun}
#' @return a fuzzy number
#'    
#' @exportMethod fapply
#' @name fapply
#' @aliases fapply,PiecewiseLinearFuzzyNumber,function-method
#' @rdname fapply-methods
#' @family PiecewiseLinearFuzzyNumber-method
#' @docType methods
setMethod(
   f="fapply",
   signature(object="PiecewiseLinearFuzzyNumber", fun="function"),
   definition=function(object, fun, ...)
   {
      ol <-     c(object@a1, object@knot.left,  object@a2)
      or <- rev(c(object@a3, object@knot.right, object@a4))
      
      ol <- fun(ol, ...)
      or <- fun(or, ...)
      
      # using the extension principle and interval-based arithmetic operations
      PiecewiseLinearFuzzyNumber(knot.alpha=object@knot.alpha,
                                 knot.left=pmin(ol, or),
                                 knot.right=rev(pmax(ol, or))
      )
   }
)

