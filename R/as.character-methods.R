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




#' Get Basic Information on a Fuzzy Number in a String
#'
#' @aliases as.character,FuzzyNumber-method
#' @rdname as.character-methods
#' @family FuzzyNumber-method
#' @export
as.character.FuzzyNumber <- function(x, ...) {
   sprintf("Fuzzy number with:\n   support=[%g,%g],\n      core=[%g,%g].\n",
               x@a1, x@a4, x@a2, x@a3)
}




#' @aliases as.character,PiecewiseLinearFuzzyNumber-method
#' @rdname as.character-methods
#' @export
as.character.PiecewiseLinearFuzzyNumber <- function(x, ...) {
   sprintf("Piecewise linear fuzzy number with %g knot(s),\n   support=[%g,%g],\n      core=[%g,%g].\n",
               x@knot.n, x@a1, x@a4, x@a2, x@a3)
}





#' @aliases as.character,TrapezoidalFuzzyNumber-method
#' @rdname as.character-methods
#' @exports
as.character.TrapezoidalFuzzyNumber <- function(x, ...) {
   sprintf("Trapezoidal fuzzy number with:\n   support=[%g,%g],\n      core=[%g,%g].\n",
               x@a1, x@a4, x@a2, x@a3)
}





#' @aliases as.character,PowerFuzzyNumber-method
#' @rdname as.character-methods
#' @export
as.character.PowerFuzzyNumber <- function(x, ...) {
   sprintf("Fuzzy number given by power functions, and:\n   support=[%g,%g],\n      core=[%g,%g].\n",
               x@a1, x@a4, x@a2, x@a3)
}
