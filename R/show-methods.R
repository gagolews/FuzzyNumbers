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



#' Print Basic Information on a Fuzzy Number
#'
#' Calls \code{\link{as.character}} (with default arguments)
#' and displays the result on the screen.
#' 
#' @param object FuzzyNumber to be printed out
#' 
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
      cat(as.character(object))
   }
)
