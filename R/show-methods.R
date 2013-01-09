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



#' Print basic information on a fuzzy number
#'
#' 
#' @section Methods:
#' \describe{
#'      \item{\code{signature(object = "FuzzyNumber")}}{  }
#'      \item{\code{signature(object = "TrapezoidalFuzzyNumber")}}{  }
#'      \item{\code{signature(object = "PiecewiseLinearFuzzyNumber")}}{  }
#'      \item{\code{signature(object = "PowerFuzzyNumber")}}{  }
#' }
#' @exportMethod show
#' @name show
#' @aliases show,FuzzyNumber-method
#' @rdname show-methods
#' @family FuzzyNumber-method
#' @docType methods
setMethod(
   f="show",
   signature(object="FuzzyNumber"),
   definition=function(object)
   {
      cat(sprintf("Fuzzy number with:\n   support=[%g,%g],\n      core=[%g,%g].\n",
                  object@a1, object@a4, object@a2, object@a3))
   }
)




#' @exportMethod show
#' @name show
#' @aliases show,PiecewiseLinearFuzzyNumber-method
#' @rdname show-methods
#' @docType methods
setMethod(
   f="show",
   signature(object="PiecewiseLinearFuzzyNumber"),
   definition=function(object) {
      cat(sprintf("Piecewise linear fuzzy number with %g knot(s),\n   support=[%g,%g],\n      core=[%g,%g].\n",
                  object@knot.n, object@a1, object@a4, object@a2, object@a3))
   }
)




#' @exportMethod show
#' @name show
#' @aliases show,TrapezoidalFuzzyNumber-method
#' @rdname show-methods
#' @docType methods
setMethod(
   f="show",
   signature(object="TrapezoidalFuzzyNumber"),
   definition=function(object)
   {
      cat(sprintf("Trapezoidal fuzzy number with:\n   support=[%g,%g],\n      core=[%g,%g].\n",
                  object@a1, object@a4, object@a2, object@a3))
   }
)




#' @exportMethod show
#' @name show
#' @aliases show,PowerFuzzyNumber-method
#' @rdname show-methods
#' @docType methods
setMethod(
   f="show",
   signature(object="PowerFuzzyNumber"),
   definition=function(object)
   {
      cat(sprintf("Fuzzy number given by power functions, and:\n   support=[%g,%g],\n      core=[%g,%g].\n",
                  object@a1, object@a4, object@a2, object@a3))
   }
)
