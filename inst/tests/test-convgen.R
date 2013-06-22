require("testthat")

test_that("converting side functions to side generators", {
   
   f1 <- convertSide(function(x) x, 0, 1)
   expect_that(f1(c(0,1)), equals(c(0,1)))
   
   f2 <- convertSide(function(x) 1-0.5*x, 0, 2)
   expect_that(f2(c(0,1)), equals(c(1,0)))
   
   f3 <- convertSide(
      splinefun(c(-4,-3.5,-3,-2.2,-2), c(0,0.4,0.7,0.9,1), method="monoH.FC"),
      -4, -2)
   expect_that(f3(c(0,1)), equals(c(0,1)))
   
   f4 <- convertSide(
      splinefun(c(-100, -50, 1, 20), c(1,0.8,0.1,0), method="monoH.FC"),
      -100, 20)
   expect_that(f4(c(0,1)), equals(c(1,0)))
   
   f5 <- convertSide(
      splinefun(c(-100, -50, 1, 20), c(1,0.8,0.1,0), method="monoH.FC"),
      20,-100)
   expect_that(f5(c(0,1)), equals(c(0,1)))
})


test_that("converting alpha-cut functions to alpha-cut generators", {
   
   f1 <- convertAlpha(function(x) x, 0, 1)
   expect_that(f1(c(0,1)), equals(c(0,1)))
   
   f2 <- convertAlpha(function(x) 2*(1-x), 0, 2)
   expect_that(f2(c(0,1)), equals(c(1,0)))
   
   f3 <- convertAlpha(
      splinefun(c(0,0.4,0.7,0.9,1), c(-4,-3.5,-3,-2.2,-2), method="monoH.FC"),
      -4, -2)
   expect_that(f3(c(0,1)), equals(c(0,1)))
   
   f4 <- convertAlpha(
      splinefun(c(1,0.8,0.1,0), c(-100, -50, 1, 20), method="monoH.FC"),
      -100, 20)
   expect_that(f4(c(0,1)), equals(c(1,0)))
   
   f5 <- convertAlpha(
      splinefun(c(1,0.8,0.1,0), c(-100, -50, 1, 20), method="monoH.FC"),
      20,-100)
   expect_that(f5(c(0,1)), equals(c(0,1)))
})


test_that("converting generators and creating FuzzyNumbers", {
   
   A <- FuzzyNumber(-1,0.5,10,100,
      lower=convertAlpha(
         splinefun(c(0,0.4,0.7,0.9,1), c(-4,-3.5,-3,-2.2,-2), method="monoH.FC"),
         -4, -2),
      upper=convertAlpha(
         splinefun(c(1,0.8,0.1,0), c(-100, -50, 1, 20), method="monoH.FC"),
         -100, 20))
   
   expect_that(A, is_a("FuzzyNumber"))
   
   B <- FuzzyNumber(-1,0.5,10,100,
                    left=convertSide(
                       splinefun(c(-4,-3.5,-3,-2.2,-2), c(0,0.4,0.7,0.9,1), method="monoH.FC"),
                       -4, -2),
                    right=convertSide(
                       splinefun(c(-100, -50, 1, 20), c(1,0.8,0.1,0), method="monoH.FC"),
                       -100, 20))
   
   expect_that(B, is_a("FuzzyNumber"))
   
})
