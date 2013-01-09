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


#' Arithmetic operations on fuzzy numbers
#' 
#' Currently implemented:
#' Fuzzy number scalar multiplication,
#' change sign, trapezoidal fuzzy number addition and subtraction.
#' 
#' TO DO: more ops
#' 
#' @return A fuzzy number
#' @exportMethod *
#' @name Arithmetic
#' @rdname Arithmetic-methods
#' @docType methods
#' @family FuzzyNumber-method
#' @aliases *,numeric,FuzzyNumber-method
setMethod("*",
   signature(e1 = "numeric", e2 = "FuzzyNumber"),
   function (e1, e2)
   {
      e2*e1
   }
)

#' @exportMethod -
#' @name Arithmetic
#' @rdname Arithmetic-methods
#' @docType methods
#' @aliases -,FuzzyNumber,ANY-method
setMethod("-",
   signature(e1 = "FuzzyNumber"),
   function (e1, e2)
   {
      e1*(-1)
   }
)





#' @exportMethod +
#' @name Arithmetic
#' @rdname Arithmetic-methods
#' @docType methods
#' @aliases +,TrapezoidalFuzzyNumber,TrapezoidalFuzzyNumber-method
setMethod("+",
          signature(e1 = "TrapezoidalFuzzyNumber", e2 = "TrapezoidalFuzzyNumber"),
          function (e1, e2)
          {
             TrapezoidalFuzzyNumber(e1@a1+e2@a1, e1@a2+e2@a2, e1@a3+e2@a3, e1@a4+e2@a4)
          }
)



#' @exportMethod -
#' @name Arithmetic
#' @rdname Arithmetic-methods
#' @docType methods
#' @aliases -,TrapezoidalFuzzyNumber,TrapezoidalFuzzyNumber-method
setMethod("-",
          signature(e1 = "TrapezoidalFuzzyNumber", e2 = "TrapezoidalFuzzyNumber"),
          function (e1, e2)
          {
             TrapezoidalFuzzyNumber(e1@a1-e2@a4, e1@a2-e2@a3, e1@a3-e2@a2, e1@a4-e2@a1)
          }
)

#' @exportMethod *
#' @name Arithmetic
#' @rdname Arithmetic-methods
#' @docType methods
#' @aliases *,TrapezoidalFuzzyNumber,numeric-method
setMethod("*",
   signature(e1 = "TrapezoidalFuzzyNumber", e2 = "numeric"),
   function (e1, e2)
   {
      stopifnot(length(e2) == 1);
      TrapezoidalFuzzyNumber(min(e2*e1@a1, e2*e1@a4),
                             min(e2*e1@a2, e2*e1@a3),
                             max(e2*e1@a2, e2*e1@a3),
                             max(e2*e1@a1, e2*e1@a4)
      )
   }
)



# setMethod("+",
#    signature(e1 = "PiecewiseLinearFuzzyNumber", e2 = "PiecewiseLinearFuzzyNumber"),
#    function (e1, e2)
#    {
#       PiecewiseLinearFuzzyNumber(e1@a1+e2@a1, e1@a2+e2@a2, e1@a3+e2@a3, e1@a4+e2@a4);
#    }
# );
#
# setMethod("-",
#    signature(e1 = "PiecewiseLinearFuzzyNumber", e2 = "PiecewiseLinearFuzzyNumber"),
#    function (e1, e2)
#    {
#       PiecewiseLinearFuzzyNumber(e1@a1-e2@a4, e1@a2-e2@a3, e1@a3-e2@a2, e1@a4-e2@a1);
#    }
# );

#' @exportMethod *
#' @name Arithmetic
#' @rdname Arithmetic-methods
#' @docType methods
#' @aliases *,PiecewiseLinearFuzzyNumber,numeric-method
setMethod("*",
          signature(e1 = "PiecewiseLinearFuzzyNumber", e2 = "numeric"),
          function (e1, e2)
          {
             stopifnot(length(e2) == 1);
             kl <-     c(e1@a1, e1@knot.left,  e1@a2);
             kr <- rev(c(e1@a3, e1@knot.right, e1@a4));
             kmin <- pmin(e2*kl, e2*kr);
             kmax <- pmax(e2*kl, e2*kr);
             
             PiecewiseLinearFuzzyNumber(
                kmin[1],
                kmin[length(kmin)],
                kmax[length(kmax)],
                kmax[1],
                knot.n=e1@knot.n,
                knot.alpha=e1@knot.alpha,
                knot.left= kmin[-c(1,length(kmin))],
                knot.right=rev(kmax[-c(1,length(kmax))])
             )
          }
)

