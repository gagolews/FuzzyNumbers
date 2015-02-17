require("testthat")

tolerance = 0.001

test_that("Case 1 of comparison examples", {
  a = as.PiecewiseLinearFuzzyNumber(TriangularFuzzyNumber(2, 3, 5))
  b = as.PiecewiseLinearFuzzyNumber(TriangularFuzzyNumber(1.5, 4, 4.8))
  
  #first we test e1>=e2 and e1>e2
  expectedPSE = 0.7777777777777777
  expectedNSE = 0.4285714285714289
  expectedPS = 0.357142857142857
  expectedNS =  0.0
  
  PSE = possibilityExceedance(a,b)
  NSE = necessityExceedance(a,b)  
  PS = possibilityStrictExceedance(a, b)
  NS = necessityStrictExceedance(a, b)

  expect_that(PSE, equals(expectedPSE, tolerance = tolerance))
  expect_that(NSE, equals(expectedNSE, tolerance = tolerance))
  expect_that(PS, equals(expectedPS, tolerance = tolerance))
  expect_that(NS, equals(expectedNS, tolerance = tolerance))
  
  #second we test e1<=e2 and e1<e2
  expectedPSE = 1.0
  expectedNSE = 0.6428571428571429
  expectedPS = 0.5714285714285711
  expectedNS =  0.22222222222222235
  
  PSE = possibilityUndervaluation(a,b)
  NSE = necessityUndervaluation(a,b)  
  PS = possibilityStrictUndervaluation(a, b)
  NS = necessityStrictUndervaluation(a, b)
  
  expect_that(PSE, equals(expectedPSE, tolerance = tolerance))
  expect_that(NSE, equals(expectedNSE, tolerance = tolerance))
  expect_that(PS, equals(expectedPS, tolerance = tolerance))
  expect_that(NS, equals(expectedNS, tolerance = tolerance))
})

test_that("Case 2 of comparison examples", {
  a = as.PiecewiseLinearFuzzyNumber(TriangularFuzzyNumber(0.2, 1.0, 2.8))
  b = as.PiecewiseLinearFuzzyNumber(TriangularFuzzyNumber(0, 1.8, 2.2))
  
  #first we test e1>=e2 and e1>e2
  expectedPSE = 0.7777777777777788
  expectedNSE = 0.3846153846153848
  expectedPS = 0.45454545454545464
  expectedNS =  0.0
  
  PSE = possibilityExceedance(a,b)
  NSE = necessityExceedance(a,b)  
  PS = possibilityStrictExceedance(a, b)
  NS = necessityStrictExceedance(a, b)
  
  expect_that(PSE, equals(expectedPSE, tolerance = tolerance))
  expect_that(NSE, equals(expectedNSE, tolerance = tolerance))
  expect_that(PS, equals(expectedPS, tolerance = tolerance))
  expect_that(NS, equals(expectedNS, tolerance = tolerance))
  
  #second we test e1<=e2 and e1<e2
  expectedPSE = 1.0
  expectedNSE = 0.5454545454545454
  expectedPS = 0.6153846153846155
  expectedNS =  0.22222222222222238

  PSE = possibilityUndervaluation(a,b)
  NSE = necessityUndervaluation(a,b)  
  PS = possibilityStrictUndervaluation(a, b)
  NS = necessityStrictUndervaluation(a, b)
  
  expect_that(PSE, equals(expectedPSE, tolerance = tolerance))
  expect_that(NSE, equals(expectedNSE, tolerance = tolerance))
  expect_that(PS, equals(expectedPS, tolerance = tolerance))
  expect_that(NS, equals(expectedNS, tolerance = tolerance))
})

test_that("Case 3 of comparison examples", {
  a = as.PiecewiseLinearFuzzyNumber(TriangularFuzzyNumber(1.7, 2.7, 2.8), knot.n = 9)
  b = as.PiecewiseLinearFuzzyNumber(TriangularFuzzyNumber(0, 1.8, 2.2), knot.n = 9)
  
  expectedPSE = 1.0
  expectedNSE = 0.9642857142857143
  expectedPS = 1.0
  expectedNS =  0.642857142857143
  
  PSE = possibilityExceedance(a,b)
  NSE = necessityExceedance(a,b)  
  PS = possibilityStrictExceedance(a, b)
  NS = necessityStrictExceedance(a, b)
  
  expect_that(PSE, equals(expectedPSE, tolerance = tolerance))
  expect_that(NSE, equals(expectedNSE, tolerance = tolerance))
  expect_that(PS, equals(expectedPS, tolerance = tolerance))
  expect_that(NS, equals(expectedNS, tolerance = tolerance))
  
  #second we test e1<=e2 and e1<e2
  expectedPSE = 0.35714285714285726
  expectedNSE = 0.0
  expectedPS = 0.03571428571428574
  expectedNS =  0.0
    
  PSE = possibilityUndervaluation(a,b)
  NSE = necessityUndervaluation(a,b)  
  PS = possibilityStrictUndervaluation(a, b)
  NS = necessityStrictUndervaluation(a, b)
  
  expect_that(PSE, equals(expectedPSE, tolerance = tolerance))
  expect_that(NSE, equals(expectedNSE, tolerance = tolerance))
  expect_that(PS, equals(expectedPS, tolerance = tolerance))
  expect_that(NS, equals(expectedNS, tolerance = tolerance))
})

test_that("Case 4 of comparison examples", {
  a = as.PiecewiseLinearFuzzyNumber(TriangularFuzzyNumber(-4.2, 1.3, 2.8))
  b = as.PiecewiseLinearFuzzyNumber(TriangularFuzzyNumber(-1, 0, 2.2))

  expectedPSE = 1.0
  expectedNSE = 0.3538461538461538
  expectedPS = 0.7567567567567571
  expectedNS =  0.1688311688311688
  
  PSE = possibilityExceedance(a,b)
  NSE = necessityExceedance(a,b)  
  PS = possibilityStrictExceedance(a, b)
  NS = necessityStrictExceedance(a, b)
  
  expect_that(PSE, equals(expectedPSE, tolerance = tolerance))
  expect_that(NSE, equals(expectedNSE, tolerance = tolerance))
  expect_that(PS, equals(expectedPS, tolerance = tolerance))
  expect_that(NS, equals(expectedNS, tolerance = tolerance))
  
  #second we test e1<=e2 and e1<e2
  expectedPSE = 0.8311688311688311
  expectedNSE = 0.24324324324324337
  expectedPS = 0.6461538461538462
  expectedNS =  0.0

  PSE = possibilityUndervaluation(a,b)
  NSE = necessityUndervaluation(a,b)  
  PS = possibilityStrictUndervaluation(a, b)
  NS = necessityStrictUndervaluation(a, b)
  
  expect_that(PSE, equals(expectedPSE, tolerance = tolerance))
  expect_that(NSE, equals(expectedNSE, tolerance = tolerance))
  expect_that(PS, equals(expectedPS, tolerance = tolerance))
  expect_that(NS, equals(expectedNS, tolerance = tolerance))
})

test_that("Case 5 of comparison examples", {
  a = as.PiecewiseLinearFuzzyNumber(TriangularFuzzyNumber(2.2, 2.7, 2.8))
  b = as.PiecewiseLinearFuzzyNumber(TriangularFuzzyNumber(0, 1.8, 2.2))
  
  expectedPSE = 1.0
  expectedNSE = 1.0
  expectedPS = 1.0
  expectedNS =  1.0
  
  PSE = possibilityExceedance(a,b)
  NSE = necessityExceedance(a,b)  
  PS = possibilityStrictExceedance(a, b)
  NS = necessityStrictExceedance(a, b)
  
  expect_that(PSE, equals(expectedPSE, tolerance = tolerance))
  expect_that(NSE, equals(expectedNSE, tolerance = tolerance))
  expect_that(PS, equals(expectedPS, tolerance = tolerance))
  expect_that(NS, equals(expectedNS, tolerance = tolerance))
  
  #second we test e1<=e2 and e1<e2
  expectedPSE = 0.0
  expectedNSE = 0.0
  expectedPS = 0.0
  expectedNS =  0.0
  
  PSE = possibilityUndervaluation(a,b)
  NSE = necessityUndervaluation(a,b)  
  PS = possibilityStrictUndervaluation(a, b)
  NS = necessityStrictUndervaluation(a, b)
  
  expect_that(PSE, equals(expectedPSE, tolerance = tolerance))
  expect_that(NSE, equals(expectedNSE, tolerance = tolerance))
  expect_that(PS, equals(expectedPS, tolerance = tolerance))
  expect_that(NS, equals(expectedNS, tolerance = tolerance))
})
