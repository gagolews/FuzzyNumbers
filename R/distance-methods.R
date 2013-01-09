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



setGeneric("distance",
           function(object1, object2, ...) standardGeneric("distance"))


#' Calculate the distance between two FNs
#'
#' This is done by numerical integration
#'
#' @param type one of \code{"Euclidean"}, \code{"EuclideanSquared"}
#' @param rel.tol numeric;
#' @section Methods:
#' \describe{
#'      \item{\code{signature(object1 = "FuzzyNumber", object2 = "FuzzyNumber")}}{}
#'      \item{\code{signature(object1 = "DiscontinuousFuzzyNumber", object2 = "FuzzyNumber")}}{}
#'      \item{\code{signature(object1 = "FuzzyNumber", object2 = "DiscontinuousFuzzyNumber")}}{}
#'      \item{\code{signature(object1 = "DiscontinuousFuzzyNumber", object2 = "DiscontinuousFuzzyNumber")}}{}
#' }
#' @return the calculated distance
#' @exportMethod distance
#' @name distance
#' @aliases distance,FuzzyNumber,FuzzyNumber-method
#' @rdname distance-methods
#' @docType methods
#' @seealso \code{\link{integrate}}, \code{\link{integrate_discont_val}}
#' @family FuzzyNumber-method
#' @references
#' Grzegorzewski P., Metrics and orders in space of fuzzy numbers,
#' Fuzzy Sets and Systems 97, 1998, pp. 83-94.\cr
setMethod(
   f="distance",
   signature(object1="FuzzyNumber", object2="FuzzyNumber"),
   definition=function(object1, object2,
                       type=c("Euclidean", "EuclideanSquared"), ...,
                       rel.tol = .Machine$double.eps^0.35)
   {
      if (is.na(object1@lower(0)) || is.na(object2@lower(0))) return(NA);
      type = match.arg(type)
      
      if (type == "Euclidean" || type == "EuclideanSquared")
      {
         dL <- integrate(function(alpha) {
            (  object1@a1+(object1@a2-object1@a1)*object1@lower(alpha)
               -object2@a1-(object2@a2-object2@a1)*object2@lower(alpha)
            )^2
         }, 0, 1, rel.tol=rel.tol, ...)$value
         
         dU <- integrate(function(alpha) {
            (  object1@a3+(object1@a4-object1@a3)*object1@upper(alpha)
               -object2@a3-(object2@a4-object2@a3)*object2@upper(alpha)
            )^2
         }, 0, 1, rel.tol=rel.tol, ...)$value
         
         if (type == "Euclidean") return (sqrt(dL+dU)) else return (dL+dU)
      } else
      {
         return(NA)
      }
   }
)


#' @exportMethod distance
#' @name distance
#' @aliases distance,DiscontinuousFuzzyNumber,DiscontinuousFuzzyNumber-method
#' @rdname distance-methods
#' @docType methods
setMethod(
   f="distance",
   signature(object1="DiscontinuousFuzzyNumber", object2="DiscontinuousFuzzyNumber"),
   definition=function(object1, object2,
      type=c("Euclidean", "EuclideanSquared"), ...)
   {
      if (is.na(object1@lower(0)) || is.na(object2@lower(0))) return(NA)
      type = match.arg(type)

      if (type == "Euclidean" || type == "EuclideanSquared")
      {
         discontL <- c(object1@discontinuities.lower, object2@discontinuities.lower)
         discontL <- unique(sort(discontL))
         dL <- integrate_discont_val(function(alpha) {
            (  object1@a1+(object1@a2-object1@a1)*object1@lower(alpha)
              -object2@a1-(object2@a2-object2@a1)*object2@lower(alpha)
            )^2
         }, 0, 1, discontinuities=discontL, ...)

         discontU <- c(object1@discontinuities.upper, object2@discontinuities.upper)
         discontU <- unique(sort(discontU))
         dU <- integrate_discont_val(function(alpha) {
            (   object1@a3+(object1@a4-object1@a3)*object1@upper(alpha)
              - object2@a3-(object2@a4-object2@a3)*object2@upper(alpha)
            )^2
         }, 0, 1, discontinuities=discontU, ...)

         if (type == "Euclidean") return (sqrt(dL+dU)) else return (dL+dU)
      } else
      {
         return(NA)
      }
   }
)



#' @exportMethod distance
#' @name distance
#' @aliases distance,FuzzyNumber,DiscontinuousFuzzyNumber-method
#' @rdname distance-methods
#' @docType methods
setMethod(
   f="distance",
   signature(object1="FuzzyNumber", object2="DiscontinuousFuzzyNumber"),
   definition=function(object1, object2,
      type=c("Euclidean", "EuclideanSquared"), ...)
   {
      stopifnot(class(object1)!="DiscontinuousFuzzyNumber")
      return(distance(object2, object1, type=type, ...))
   }
)


#' @exportMethod distance
#' @name distance
#' @aliases distance,DiscontinuousFuzzyNumber,FuzzyNumber-method
#' @rdname distance-methods
#' @docType methods
setMethod(
   f="distance",
   signature(object1="DiscontinuousFuzzyNumber", object2="FuzzyNumber"),
   definition=function(object1, object2,
      type=c("Euclidean", "EuclideanSquared"), ...)
   {
      stopifnot(class(object2)!="DiscontinuousFuzzyNumber");
      if (is.na(object1@lower(0)) || is.na(object2@lower(0))) return(NA)
      type = match.arg(type)

      if (type == "Euclidean" || type == "EuclideanSquared")
      {
         dL <- integrate_discont_val(function(alpha) {
            (  object1@a1+(object1@a2-object1@a1)*object1@lower(alpha)
              -object2@a1-(object2@a2-object2@a1)*object2@lower(alpha)
            )^2
         }, 0, 1, discontinuities=object1@discontinuities.lower, ...)

         dU <- integrate_discont_val(function(alpha) {
            (  object1@a3+(object1@a4-object1@a3)*object1@upper(alpha)
              -object2@a3-(object2@a4-object2@a3)*object2@upper(alpha)
            )^2
         }, 0, 1, discontinuities=object1@discontinuities.upper, ...)

         if (type == "Euclidean") return (sqrt(dL+dU)) else return (dL+dU)
      } else
      {
         return(NA)
      }
   }
)


