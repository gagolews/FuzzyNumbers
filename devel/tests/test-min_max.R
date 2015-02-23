require("testthat")

test_that("Minimum - Maximum Fuzzy nubmers", {
  
  # situation with clear definition of min and max
  x1 = as.PiecewiseLinearFuzzyNumber(TriangularFuzzyNumber(-3, -2 , -1))
  x2 = as.PiecewiseLinearFuzzyNumber(TriangularFuzzyNumber(1, 2, 3))
  maxFN = maximum(x1,x2)
  minFN = minimum(x1,x2)
  expect_that(minFN, equals(x1))
  expect_that(maxFN, equals(x2))
  
  # not so clear definition of min and max
  x1 = as.PiecewiseLinearFuzzyNumber(TriangularFuzzyNumber(0, 1.5 , 5))
  x2 = as.PiecewiseLinearFuzzyNumber(TriangularFuzzyNumber(-0.5, 2, 3))
  maxFN = maximum(x1,x2)
  minFN = minimum(x1,x2)
  expectedMin = as.PiecewiseLinearFuzzyNumber(TriangularFuzzyNumber(-0.5, 1.5 , 3))
  expectedMax = as.PiecewiseLinearFuzzyNumber(TriangularFuzzyNumber(0, 2, 5))
  expect_that(minFN, equals(expectedMin))
  expect_that(maxFN, equals(expectedMax))
  
  # check if 0.5 cut is determined correctly
  x1 = as.PiecewiseLinearFuzzyNumber(TriangularFuzzyNumber(0, 1.5 , 5), knot.n = 1)
  x2 = as.PiecewiseLinearFuzzyNumber(TriangularFuzzyNumber(-1, 2, 3), knot.n = 1)
  maxFN = maximum(x1,x2)
  minFN = minimum(x1,x2)
  expectedMin = PiecewiseLinearFuzzyNumber(-1, 1.5, 1.5, 3, knot.alpha = c(0.5), 
                                           knot.left = c(0.5),
                                           knot.right = c(2.5))
  expectedMax = PiecewiseLinearFuzzyNumber(0, 2, 2, 5, knot.alpha = c(0.5), 
                                           knot.left = c(0.75),
                                           knot.right = c(3.25))
  
  expect_that(alphacut(minFN,0.5), equals(alphacut(expectedMin,0.5)))
  expect_that(alphacut(maxFN,0.5), equals(alphacut(expectedMax,0.5)))
})