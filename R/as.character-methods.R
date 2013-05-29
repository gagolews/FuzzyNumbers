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
#'
#' @param x FuzzyNumber to be described
#' @param toLaTeX logical; should LaTeX code be output?
#' @param varnameLaTeX character; variable name to be included in equations
#' 
#' @return character vector
#' 
#' @details
#' Thanks to Jan Caha for suggesting the \code{toLaTeX} arg.
#' 
#' @aliases as.character,FuzzyNumber-method
#' @rdname as.character-methods
#' @family FuzzyNumber-method
#' @export
as.character.FuzzyNumber <- function(x, toLaTeX=FALSE, varnameLaTeX="A", ...) {
   if (identical(toLaTeX, FALSE)) {
      sprintf("Fuzzy number with:\n   support=[%g,%g],\n      core=[%g,%g].\n",
              x@a1, x@a4, x@a2, x@a3)
   }
   else {
      res <- sprintf(
"\\[
\\mu_{%s}(x) = \\left\\{
\\begin{array}{lll}
0      & \\text{for} & x\\in(-\\infty,%g), \\\\
l_{%s}(x) & \\text{for} & x\\in[%g,%g), \\\\
1      & \\text{for} & x\\in[%g,%g], \\\\
r_{%s}(x) & \\text{for} & x\\in(%g,%g], \\\\
0      & \\text{for} & x\\in(%g,+\\infty), \\\\
\\end{array}
\\right.
\\]",
         varnameLaTeX,
         x@a1,
         varnameLaTeX, x@a1, x@a2,
         x@a2, x@a3,
         varnameLaTeX, x@a3, x@a4,
         x@a4
      )
      
      res <- paste(res, sprintf(
"where $l_{%s}=\\mathtt{left}_A((x%+g)/%g)$,
$r_{%s}=\\mathtt{right}_A((x%+g)/%g)$.", 
        varnameLaTeX, -x@a1, x@a2-x@a1,
        varnameLaTeX, -x@a3, x@a4-x@a3
      ), collapse="\n")
      
      res <- paste(res, sprintf(
"\\[
{%s}_\\alpha = [{%s}_L(\\alpha), {%s}_U(\\alpha)],
\\]
where ${%s}_L(\\alpha)=%g%+g\\,\\mathtt{lower}_{%s}(\\alpha)$,
      ${%s}_U(\\alpha)=%g%+g\\,\\mathtt{upper}_{%s}(\\alpha)$.",
        varnameLaTeX, varnameLaTeX, varnameLaTeX, 
        varnameLaTeX, x@a1, x@a2-x@a1, varnameLaTeX, 
        varnameLaTeX, x@a3, x@a4-x@a3, varnameLaTeX
      ), collapse="\n\n")
      
      res
   }
}





#' @aliases as.character,PiecewiseLinearFuzzyNumber-method
#' @rdname as.character-methods
#' @export
as.character.PiecewiseLinearFuzzyNumber <- function(x, toLaTeX=FALSE, varnameLaTeX="A", ...) {
   sprintf("Piecewise linear fuzzy number with %g knot(s),\n   support=[%g,%g],\n      core=[%g,%g].\n",
               x@knot.n, x@a1, x@a4, x@a2, x@a3)
   
}





#' @aliases as.character,TrapezoidalFuzzyNumber-method
#' @rdname as.character-methods
#' @export
as.character.TrapezoidalFuzzyNumber <- function(x, toLaTeX=FALSE, varnameLaTeX="A", ...) {
   if (identical(toLaTeX, FALSE)) {
      sprintf("Trapezoidal fuzzy number with:\n   support=[%g,%g],\n      core=[%g,%g].\n",
              x@a1, x@a4, x@a2, x@a3)
   }
   else {
      res <- sprintf(
"\\[
\\mu_{%s}(x) = \\left\\{
\\begin{array}{lll}
0      & \\text{for} & x\\in(-\\infty,%g), \\\\
(x%+g)/%g & \\text{for} & x\\in[%g,%g), \\\\
1      & \\text{for} & x\\in[%g,%g], \\\\
(%g-x)/%g & \\text{for} & x\\in(%g,%g], \\\\
0      & \\text{for} & x\\in(%g,+\\infty). \\\\
\\end{array}
\\right.
\\]", 
         varnameLaTeX,
         x@a1, 
         -x@a1, x@a2-x@a1, x@a1, x@a2, 
         x@a2, x@a3, 
         x@a4, x@a4-x@a3, x@a3, x@a4, 
         x@a4
      )
      
      res <- paste(res, sprintf(
"\\[
{%s}_\\alpha = [%g%+g\\,\\alpha, %g%+g\\,\\alpha].
\\]",
         varnameLaTeX,
         x@a1, x@a2-x@a1,
         x@a4, -(x@a4-x@a3)
      ), collapse="\n\n")
      
      res
   }
}





#' @aliases as.character,PowerFuzzyNumber-method
#' @rdname as.character-methods
#' @export
as.character.PowerFuzzyNumber <- function(x, toLaTeX=FALSE, varnameLaTeX="A", ...) {
   sprintf("Fuzzy number given by power functions, and:\n   support=[%g,%g],\n      core=[%g,%g].\n",
               x@a1, x@a4, x@a2, x@a3)
}




