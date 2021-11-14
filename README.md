# **FuzzyNumbers**

### Tools to Deal with Fuzzy Numbers in R

**FuzzyNumbers** is an open source (LGPL 3) package for R. It provides S4
classes and methods to deal with fuzzy numbers. The package may be used
by researchers in FN theory, e.g., for preparing figures,
generating numerical examples, or testing new algorithms.

Fuzzy set theory gives one of many ways (in particular, see Bayesian
probabilities) to represent imprecise
information. Fuzzy numbers form a particular subclass of fuzzy sets
of the real line. The main idea behind this concept is motivated by
the observation that people tend to describe their knowledge
about objects through vague numbers, e.g., "I'm about 180 cm tall" or
"The event happened between 2 and 3 p.m.".

The **FuzzyNumbers** package aims to provide the following functionality:

* Representation of arbitrary fuzzy numbers (including FNs with
discontinuous side functions and/or alpha-cuts), as well as their particular
types, e.g., trapezoidal and piecewise linear fuzzy numbers,

* Defuzzification and approximation of FNs by triangular
and piecewise linear fuzzy numbers,

* Visualization,

* Basic arithmetic operations,

* Ranking of fuzzy numbers (implemented operators <=, <, >, >= in forms
of possibility and necessity functions - works only for
`PiecewiseLinearFuzzyNumbers` for now),

* and many more.

* * *

Package record on **CRAN**: http://cran.r-project.org/web/packages/FuzzyNumbers/

**Tutorial**:
Gagolewski M., Caha J., [A Guide to the FuzzyNumbers Package for R](https://cran.r-project.org/web/packages/FuzzyNumbers/vignettes/FuzzyNumbersTutorial.pdf), 2019.


* * *

**Author**: [Marek Gagolewski](https://www.gagolewski.com/), with contributions from Jan Caha

To **cite** package **FuzzyNumbers** in publications please use:

> "Gagolewski M., Caha J. (2021).
*FuzzyNumbers Package: Tools to deal with fuzzy numbers in R*.
https://github.com/gagolews/FuzzyNumbers/."

A BibTeX entry for LaTeX users is:

```
@Manual{FuzzyNumbersPkgR,
   title = {FuzzyNumbers Package: Tools to deal with fuzzy numbers in R},
   author = {Marek Gagolewski},
   year = {2021},
   url = {https://github.com/gagolews/FuzzyNumbers/}
}
```
