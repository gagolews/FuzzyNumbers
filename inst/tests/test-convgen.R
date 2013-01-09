require("testthat")

test_that("converting side functions to side generators", {
   
   f1 <- convert.side(function(x) x, 0, 1)
   expect_that(f1(c(0,1)), equals(c(0,1)))
   
   f2 <- convert.side(function(x) 1-0.5*x, 0, 2)
   expect_that(f2(c(0,1)), equals(c(1,0)))
   
   f3 <- convert.side(
      splinefun(c(-4,-3.5,-3,-2.2,-2), c(0,0.4,0.7,0.9,1), method="monoH.FC"),
      -4, -2)
   expect_that(f3(c(0,1)), equals(c(0,1)))
   
   f4 <- convert.side(
      splinefun(c(-100, -50, 1, 20), c(1,0.8,0.1,0), method="monoH.FC"),
      -100, 20)
   expect_that(f4(c(0,1)), equals(c(1,0)))
   
   f5 <- convert.side(
      splinefun(c(-100, -50, 1, 20), c(1,0.8,0.1,0), method="monoH.FC"),
      20,-100)
   expect_that(f5(c(0,1)), equals(c(0,1)))
})


test_that("converting alpha-cut functions to alpha-cut generators", {
   
   f1 <- convert.alpha(function(x) x, 0, 1)
   expect_that(f1(c(0,1)), equals(c(0,1)))
   
   f2 <- convert.alpha(function(x) 2*(1-x), 0, 2)
   expect_that(f2(c(0,1)), equals(c(1,0)))
   
   f3 <- convert.alpha(
      splinefun(c(0,0.4,0.7,0.9,1), c(-4,-3.5,-3,-2.2,-2), method="monoH.FC"),
      -4, -2)
   expect_that(f3(c(0,1)), equals(c(0,1)))
   
   f4 <- convert.alpha(
      splinefun(c(1,0.8,0.1,0), c(-100, -50, 1, 20), method="monoH.FC"),
      -100, 20)
   expect_that(f4(c(0,1)), equals(c(1,0)))
   
   f5 <- convert.alpha(
      splinefun(c(1,0.8,0.1,0), c(-100, -50, 1, 20), method="monoH.FC"),
      20,-100)
   expect_that(f5(c(0,1)), equals(c(0,1)))
})


test_that("converting generators and creating FuzzyNumbers", {
   
   A <- FuzzyNumber(-1,0.5,10,100,
      lower=convert.alpha(
         splinefun(c(0,0.4,0.7,0.9,1), c(-4,-3.5,-3,-2.2,-2), method="monoH.FC"),
         -4, -2),
      upper=convert.alpha(
         splinefun(c(1,0.8,0.1,0), c(-100, -50, 1, 20), method="monoH.FC"),
         -100, 20))
   
   expect_that(A, is_a("FuzzyNumber"))
   
   B <- FuzzyNumber(-1,0.5,10,100,
                    left=convert.side(
                       splinefun(c(-4,-3.5,-3,-2.2,-2), c(0,0.4,0.7,0.9,1), method="monoH.FC"),
                       -4, -2),
                    right=convert.side(
                       splinefun(c(-100, -50, 1, 20), c(1,0.8,0.1,0), method="monoH.FC"),
                       -100, 20))
   
   expect_that(B, is_a("FuzzyNumber"))
   
})
