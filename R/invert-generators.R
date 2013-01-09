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




#' Approximate the inverse of a given side generating functions using interpolation
#' 
#' The function is a wrapper to splinefun() and approxfun().
#' It may be used to create side generating functions from
#' alpha-cut generators and inversely.
#' 
#' @param f a monotonic, continuous function f: [0,1]->[0,1]
#' @param method interpolation method: "monoH.FC", "hyman" or "linear"
#' @param n number of interpolation points
#' @return a new function, the approximate inverse of the input
#' @seealso \code{\linkS4class{FuzzyNumber}}
#' @export
approx.invert <- function(f, method=c("monoH.FC", "linear", "hyman"), n=500) 
{
   stopifnot(is.function(f))
   method <- match.arg(method)
   x <- seq(0, 1, length.out=n)
   y <- pmax(0, pmin(1, f(x)))
   stopifnot(all(is.finite(y)))
   stopifnot(!is.unsorted(y, strictly=TRUE) || !is.unsorted(rev(y), strictly=TRUE))
   if (method == "linear")
      approxfun(y, x, method="linear")
   else
      splinefun(y, x, method=method)
}

