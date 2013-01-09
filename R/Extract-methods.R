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



#' FuzzyNumber slot accessor (read-only)
#'
#' For possible slot names see man pages for class FuzzyNumber and its derivatives
#' 
#' @param i slot name
#' @param j not used
#' @param drop not used
#' @return slot value
#' @exportMethod [
#' @name Extract
#' @rdname Extract-methods
#' @docType methods
#' @aliases [,FuzzyNumber-method
#' @family FuzzyNumber-method
#' @examples
#' A <- FuzzyNumber(1,2,3,4)
#' A["a1"]
#' A["right"]
setMethod(
   f="[",
   signature=(x="FuzzyNumber"),
   definition=function(x, i, j, drop)
   {
      if (i == "a1") return(x@a1)
      if (i == "a2") return(x@a2)
      if (i == "a3") return(x@a3)
      if (i == "a4") return(x@a4)
      if (i == "left")  return(x@left)
      if (i == "right") return(x@right)
      if (i == "lower") return(x@lower)
      if (i == "upper") return(x@upper)
   }
)




#' @exportMethod [
#' @name Extract
#' @rdname Extract-methods
#' @docType methods
#' @aliases [,PiecewiseLinearFuzzyNumber-method
setMethod(
   f="[",
   signature=(x="PiecewiseLinearFuzzyNumber"),
   definition=function(x, i, j, drop)
   {
      if (i == "knot.n")     return(x@knot.n)
      if (i == "knot.alpha") return(x@knot.alpha)
      if (i == "knot.left")  return(x@knot.left)
      if (i == "knot.right") return(x@knot.right)
      if (i == "knots")      return(matrix(c(x@knot.alpha, x@knot.left, rev(x@knot.right)),
                                           ncol=3,
                                           dimnames=list(
                                              paste("knot_", 1:x@knot.n, sep=""),
                                              c("alpha", "left", "right")
                                           )
      ))
      if (i == "allknots")   return(matrix(c(0,x@knot.alpha,1,  x@a1, x@knot.left, x@a2, x@a4, rev(x@knot.right), x@a3),
                                           ncol=3,
                                           dimnames=list(
                                              c("supp", paste("knot_", 1:x@knot.n, sep=""), "core"),
                                              c("alpha", "left", "right")
                                           )
      ))
      
      if (i == "a1") return(x@a1)
      if (i == "a2") return(x@a2)
      if (i == "a3") return(x@a3)
      if (i == "a4") return(x@a4)
      if (i == "left")  return(x@left)
      if (i == "right") return(x@right)
      if (i == "lower") return(x@lower)
      if (i == "upper") return(x@upper)
      
      #     return(callNextMethod()) # does not work...
   }
)


#' @exportMethod [
#' @name Extract
#' @rdname Extract-methods
#' @docType methods
#' @aliases [,PowerFuzzyNumber-method
setMethod(
   f="[",
   signature=(x="PowerFuzzyNumber"),
   definition=function(x, i, j, drop)
   {
      if (i == "a1") return(x@a1)
      if (i == "a2") return(x@a2)
      if (i == "a3") return(x@a3)
      if (i == "a4") return(x@a4)
      if (i == "left")  return(x@left)
      if (i == "right") return(x@right)
      if (i == "lower") return(x@lower)
      if (i == "upper") return(x@upper)
      if (i == "p.left") return(x@p.left)
      if (i == "p.right") return(x@p.right)
   }
)





#' @exportMethod [
#' @name Extract
#' @rdname Extract-methods
#' @docType methods
#' @aliases [,DiscontinuousFuzzyNumber-method
setMethod(
   f="[",
   signature=(x="DiscontinuousFuzzyNumber"),
   definition=function(x, i, j, drop)
   {
      if (i == "a1") return(x@a1)
      if (i == "a2") return(x@a2)
      if (i == "a3") return(x@a3)
      if (i == "a4") return(x@a4)
      if (i == "left")  return(x@left)
      if (i == "right") return(x@right)
      if (i == "lower") return(x@lower)
      if (i == "upper") return(x@upper)
      if (i == "discontinuities.left")  return(x@discontinuities.left)
      if (i == "discontinuities.right") return(x@discontinuities.right)
      if (i == "discontinuities.lower") return(x@discontinuities.lower)
      if (i == "discontinuities.upper") return(x@discontinuities.upper)
   }
)
