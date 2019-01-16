# **FuzzyNumbers**

### Tools to Deal with Fuzzy Numbers in R

[![Build Status](https://travis-ci.org/gagolews/FuzzyNumbers.svg?branch=master)](https://travis-ci.org/gagolews/FuzzyNumbers)

**FuzzyNumbers** is an open source (LGPL 3) package for R. It provides S4
classes and methods to deal with fuzzy numbers. The package may be used
by researchers in FN theory, e.g., for preparing figures,
generating numerical examples, or testing new algorithms.

Fuzzy set theory give one of many ways to represent imprecise or vague
information. Fuzzy numbers, which form a particular subclass of fuzzy sets
of the real line, play a significant role in many important both theoretical
and practical considerations. This is because we often describe our knowledge
about objects through numbers, e.g., "I'm about 180 cm tall" or
"The event happened between 2 and 3 p.m.".

The **FuzzyNumbers** package aims to provide the following functionality:

* Representation of arbitrary fuzzy numbers (including FNs with
discontinuous side functions and/or alpha-cuts), as well as their particular
types, e.g., trapezoidal and piecewise linear fuzzy numbers,

* Defuzzification and approximation of FNs by triangular
and piecewise linear fuzzy numbers,

* Visualization,

* Basic arithmetic operations,

* Ranking of fuzzy numbers (implemented operators <=,<,>,>= in forms
of possibility and necessity functions - works only for
`PiecewiseLinearFuzzyNumbers` for now)

* and many more.

* * *

**Homepage**: http://FuzzyNumbers.rexamine.com/

Package record on **CRAN**: http://cran.r-project.org/web/packages/FuzzyNumbers/

**Tutorial**:
Gagolewski M., Caha J., [A Guide to the FuzzyNumbers Package for R](https://github.com/Rexamine/FuzzyNumbers/raw/master/devel/tutorial/FuzzyNumbers-Tutorial_current.pdf), 2019.

**On-line manual**: http://docs.rexamine.com/R-man/FuzzyNumbers/FuzzyNumbers-package.html

* * *

**Author**: [Marek Gagolewski](http://gagolewski.rexamine.com/), with contributions from Jan Caha

To **cite** package **FuzzyNumbers** in publications please use:

> "Gagolewski M., Caha J. (2019). *FuzzyNumbers Package: Tools to deal with fuzzy numbers in R*.
http://FuzzyNumbers.rexamine.com/."

A BibTeX entry for LaTeX users is:

```
@Manual{FuzzyNumbersPkgR,
   title = {FuzzyNumbers Package: Tools to deal with fuzzy numbers in R},
   author = {Marek Gagolewski},
   year = {2019},
   url = {http://FuzzyNumbers.rexamine.com/}
}
```
