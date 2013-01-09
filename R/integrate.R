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
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
## GNU Lesser General Public License for more details.
##
## You should have received a copy of the GNU Lesser General Public License
## along with FuzzyNumbers. If not, see <http://www.gnu.org/licenses/>.








#' Integrate a function with at most finite number of discontinuities
#'
#' The function uses multiple calls to \code{\link{integrate}}.
#'
#' @param f an R function taking a numeric vector of length 1 as its first
#'         argument and returning a numeric vector of length 1
#' @param from the lower limit of integration
#' @param to the upper limit of integration
#' @param discontinuities nondecreasingly sorted numeric vector which indicates
#'          the points at which \code{f} is discontinuous
#' @param rel.tol relative accuracy requested
#' @param ... further arguments to be passed to the \code{\link{integrate}} function.
#' @return the estimate of the integral
#' @export
integrate_discont_val <- function(f, from, to, discontinuities=numeric(0),
   rel.tol = .Machine$double.eps^0.35, ...)

{
   if (!is.numeric(discontinuities))
      stop("`discontinuities' should be numeric")

   stopifnot(from <= to)

   discontinuities <- discontinuities[discontinuities > from & discontinuities < to]
   m <- length(discontinuities)

   if (m == 0)
      return(integrate(f=f, lower=from, upper=to, ..., rel.tol=rel.tol)$value)

   if (is.unsorted(discontinuities))
      stop("`discontinuities' should be ordered nondecreasingly")

   x <- c(from, discontinuities, to)
   v <- numeric(m+1)

   for (i in 1:(m+1))
   {
      v[i] <- integrate(f=f, lower=x[i]+rel.tol, upper=x[i+1]-rel.tol, ...,
         rel.tol=rel.tol*(m+1))$value
   }

   return(sum(v))
}








setGeneric("integrateAlpha",
     function(object, which, from, to, ...) standardGeneric("integrateAlpha"))



#' Numerically integrate a transformed or weighted lower or upper alpha-cut bound of a fuzzy number
#'
#' @param weight a function or NULL
#' @param transform a function or NULL
#' @param rel.tol numeric
#' @section Methods:
#' \describe{
#'      \item{\code{signature(object = "FuzzyNumber", which="character", from="numeric", to="numeric")}}{   }
#'      \item{\code{signature(object = "DiscontinuousFuzzyNumber", which="character", from="numeric", to="numeric")}}{   }
#' }
#' @exportMethod integrateAlpha
#' @name integrateAlpha
#' @aliases integrateAlpha,FuzzyNumber,character,numeric,numeric-method
#' @rdname integrateAlpha-methods
#' @docType methods
#' @family FuzzyNumber-method
#' @seealso \code{\link{integrate_discont_val}}
setMethod(
   f="integrateAlpha",
   signature(object="FuzzyNumber", which="character",
             from="numeric",       to="numeric"),
   definition=function(object, which=c("lower","upper"),
                       from=0, to=1, weight=NULL, transform=NULL,
                       rel.tol = .Machine$double.eps^0.35, ...)
   {
      which <- match.arg(which);
      
      if (length(from) != 1 || length(to) != 1 || from < 0 || to > 1)
         stop("invalid `from' or `to'");
      
      if (!is.null(weight) && (class(weight) != "function" || length(formals(weight)) != 1))
         stop("`weight' should be a function with 1 parameter");
      
      if (!is.null(transform) && (class(transform) != "function" || length(formals(transform)) != 2))
         stop("`transform' should be a function with 2 parameter");
      
      if (!is.null(weight) && !is.null(transform))
         stop("specify either `weight', `transform' or none");
      
      if (which == "lower")
      {
         if (!is.null(weight))
         {
            fun <- function(alpha)
               (object@a1+(object@a2-object@a1)*object@lower(alpha))*weight(alpha);
         } else if (!is.null(transform))
         {
            fun <- function(alpha)
               transform(alpha, object@a1+(object@a2-object@a1)*object@lower(alpha));
         } else
         {
            fun <- function(alpha)
               object@a1+(object@a2-object@a1)*object@lower(alpha);
         }
      } else
      {
         if (!is.null(weight))
         {
            fun <- function(alpha)
               (object@a3+(object@a4-object@a3)*object@upper(alpha))*weight(alpha);
         } else if (!is.null(transform))
         {
            fun <- function(alpha)
               transform(alpha, object@a3+(object@a4-object@a3)*object@upper(alpha));
         } else
         {
            fun <- function(alpha)
               object@a3+(object@a4-object@a3)*object@upper(alpha);
         }
      }
      
      return(integrate(f=fun, from, to, rel.tol=rel.tol, ...)$value);
   }
);



#' @exportMethod integrateAlpha
#' @name integrateAlpha
#' @aliases integrateAlpha,DiscontinuousFuzzyNumber,character,numeric,numeric-method
#' @rdname integrateAlpha-methods
#' @docType methods
setMethod(
   f="integrateAlpha",
   signature(object="DiscontinuousFuzzyNumber", which="character",
             from="numeric",       to="numeric"),
   definition=function(object, which=c("lower","upper"),
      from=0, to=1, weight=NULL, transform=NULL,
      rel.tol = .Machine$double.eps^0.35, ...)
   {
      which <- match.arg(which)

      if (length(from) != 1 || length(to) != 1 || from < 0 || to > 1)
         stop("invalid `from' or `to'")

      if (!is.null(weight) && (class(weight) != "function" || length(formals(weight)) != 1))
         stop("`weight' should be a function with 1 parameter")

      if (!is.null(transform) && (class(transform) != "function" || length(formals(transform)) != 2))
         stop("`transform' should be a function with 2 parameter")

      if (!is.null(weight) && !is.null(transform))
         stop("specify either `weight', `transform' or none")

      if (which == "lower")
      {
         if (!is.null(weight))
         {
            fun <- function(alpha)
               (object@a1+(object@a2-object@a1)*object@lower(alpha))*weight(alpha)
         } else if (!is.null(transform))
         {
            fun <- function(alpha)
               transform(alpha, object@a1+(object@a2-object@a1)*object@lower(alpha))
         } else
         {
            fun <- function(alpha)
               object@a1+(object@a2-object@a1)*object@lower(alpha)
         }

         disconts <- object@discontinuities.lower

      } else
      {
         if (!is.null(weight))
         {
            fun <- function(alpha)
               (object@a3+(object@a4-object@a3)*object@upper(alpha))*weight(alpha)
         } else if (!is.null(transform))
         {
            fun <- function(alpha)
               transform(alpha, object@a3+(object@a4-object@a3)*object@upper(alpha))
         } else
         {
            fun <- function(alpha)
               object@a3+(object@a4-object@a3)*object@upper(alpha)
         }

         disconts <- object@discontinuities.upper
      }

      return(integrate_discont_val(fun, from, to,
         discontinuities=disconts, rel.tol=rel.tol, ...))
   }
)

