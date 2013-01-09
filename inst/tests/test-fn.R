require("testthat")

test_that("FuzzyNumber A-side functions", {
   A <- FuzzyNumber(1, 2, 4, 7,
     left  = function(x) x,
     right = function(x) 1-x
   )
   
   expect_that(A, is_a("FuzzyNumber"))
   expect_that(supp(A), equals(c(1,7)))
   expect_that(core(A), equals(c(2,4)))
   expect_that(evaluate(A, c(-1,1,2,3,4,7,10)), equals(c(0,0,1,1,1,0,0)))
})

test_that("FuzzyNumber B-alpha cuts", {
   B <- FuzzyNumber(-1, 3, 3, 9,
     lower = function(alpha) pbeta(alpha, 5, 9),
     upper = function(alpha) pexp(1/alpha-1)
   )
   
   expect_that(B, is_a("FuzzyNumber"))
   expect_that(supp(B), equals(c(-1,9)))
   expect_that(core(B), equals(c(3,3)))
   expect_that(alphacut(B, c(0,1)), equals(matrix(c(-1,3,9,3), nrow=2)))
})
  
test_that("FuzzyNumber C-shadowed set", {
   C <- FuzzyNumber(-5,-1,1,5) # shadowed set
   
   expect_that(C, is_a("FuzzyNumber"))
   expect_that(supp(C), equals(c(-5,5)))
   expect_that(core(C), equals(c(-1,1)))
})


test_that("FuzzyNumber wrong ones", {
   
   expect_that(try(FuzzyNumber(0,1,2,Inf), TRUE), is_a("try-error"))
   expect_that(try(FuzzyNumber(0,1,0,3), TRUE), is_a("try-error"))
   expect_that(try(FuzzyNumber(0,1,2,3,
                               left=function(x) 1-x,
                               right=function(x) x), TRUE), is_a("try-error"))
   expect_that(try(FuzzyNumber(0,1,2,3,
                               left=function(x) x[1],
                               right=function(x) 1-x[1]), TRUE), is_a("try-error"))
})
