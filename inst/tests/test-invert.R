require("testthat")

test_that("inverting side/alpha-cut generators numerically", {
   
   x <- seq(0,1,by=0.001)
   
   fi <- approx.invert(function(x) x)
   expect_that(fi(x), equals(x))
   
   fi <- approx.invert(function(x) x^2)
   expect_that(fi(x), equals(sqrt(x)))
   
   fi <- approx.invert(function(x) x^3)
   expect_that(fi(x), equals(x^(1/3)))
   
   fi <- approx.invert(function(x) pbeta(x, 1, 2))
   expect_that(fi(x), equals(qbeta(x, 1, 2)))
   
   fi <- approx.invert(function(x) 1-pbeta(x, 1, 2))
   expect_that(fi(x), equals(qbeta(1-x, 1, 2)))
   
#    fi <- approx.invert(function(x) pbeta(x, 0.3, 1))
#    expect_that(fi(x), equals(qbeta(x, 0.3, 1)))
})

