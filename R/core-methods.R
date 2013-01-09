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


setGeneric("core",
           function(object) standardGeneric("core"))




#' Calculate the core of a fuzzy number
#'
#' We have \eqn{supp(A) := [a2,a3]}.
#' 
#' @section Methods:
#' \describe{
#'      \item{\code{signature(object = "FuzzyNumber")}}{
#'      
#'   }
#' }
#' @exportMethod core
#' @name core
#' @aliases core,FuzzyNumber-method
#' @rdname core-methods
#' @family FuzzyNumber-method
#' @docType methods
setMethod(
   f="core",
   signature(object="FuzzyNumber"),
   definition=function(object)
   {
      c(object@a2, object@a3)
   }
)

