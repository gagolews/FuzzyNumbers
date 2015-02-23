require("testthat")

test_that("Tests of power function", {
  # supress multiplication warnings
  options(warn=-1)
  
  ######################################################
  # negative fuzzy number example - odd and even example
  x = as.PiecewiseLinearFuzzyNumber(TriangularFuzzyNumber(-3, -2 , -1))
  x2 = x^2
  expectedX2 = x*x
  expect_that(x2, equals(expectedX2))
  
  x3 = x^3
  expectedX3 = x*x*x
  expect_that(x3, equals(expectedX3))
    
  #######################################################
  # possitive fuzzy number example - odd and even example
  x = as.PiecewiseLinearFuzzyNumber(TriangularFuzzyNumber(1, 2 , 3))
  x2 = x^2
  expectedX2 = x*x
  expect_that(x2, equals(expectedX2))
  
  x3 = x^3
  expectedX3 = x*x*x
  
  expect_that(x3, equals(expectedX3))
  
  #######################################################
  # fuzzy number with zero example - odd and even example
  x <- as.PiecewiseLinearFuzzyNumber(TriangularFuzzyNumber(-1.5, 1.5 , 3), knot.n = 1)
  x2 <- x^2
  #prepare left side of fuzzy number including names of alpha cuts
  left <- c(0,x@knot.left,x@a2)
  names(left) <- c("0.0","0.5","1.0")
  expectedX2 <- PiecewiseLinearFuzzyNumber(knot.alpha=x@knot.alpha,
                                          knot.left=left**2,
                                          knot.right=c(x@a3,x@knot.right,x@a4)**2)
  expect_that(x2, equals(expectedX2))
  
  x3 = x^3
  expectedX3 = PiecewiseLinearFuzzyNumber(knot.alpha=x@knot.alpha,
                                          knot.left=c(x@a1,x@knot.left,x@a2)**3,
                                          knot.right=c(x@a3,x@knot.right,x@a4)**3)
  expect_that(x3, equals(expectedX3))
  
  # turn the warnings on again
  options(warn=0)
})

test_that("Tests of atan2 function", {
 
  # example 1
  y = as.PiecewiseLinearFuzzyNumber(TriangularFuzzyNumber(-2, 3 , 5))
  x = as.PiecewiseLinearFuzzyNumber(TriangularFuzzyNumber(-4.8, -4, 1.5))
  result = arctan2(y,x)
  expectedResult = as.PiecewiseLinearFuzzyNumber(TriangularFuzzyNumber(-pi, atan2(y@a2,x@a2), pi))
  expect_that(result, equals(expectedResult))
  
  # example 2 known example
  y = as.PiecewiseLinearFuzzyNumber(TriangularFuzzyNumber(-2, 3, 5))
  x = as.PiecewiseLinearFuzzyNumber(TriangularFuzzyNumber(-1.5, -1, -0.5))
  result = arctan2(y,x) 
  expectedResult = as.PiecewiseLinearFuzzyNumber(TriangularFuzzyNumber(-4.61272, -4.39064, -1.81577))
  # testing against user defined values so a tolerance is required
  expect_that(result, equals(expectedResult, tolerance = 0.00001))
})