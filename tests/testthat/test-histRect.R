context("Maximal area under histogram")


test_that("Simple edge cases", {
  ans<-maxHist(0, 1)
  testthat::expect_equal(ans, list(x0=1L, x1=1L, area=0))
  ans<-maxHist(c(0,0), c(1,1))
  testthat::expect_equal(ans$area, 0)
  ans<-maxHist(c(0,1), c(1,0))
  testthat::expect_equal(ans$area, 0)
  ans<-maxHist(c(1,0), c(0,1))
  testthat::expect_equal(ans$area, 0)
  ans<-maxHist(c(1,1), c(1,2))
  testthat::expect_equal(ans$area, list(x0=1L, x1=2L, area=2))
})

test_that("Test from https://www.geeksforgeeks.org/largest-rectangle-under-histogram", {
  row<-c(6,2,5,4,5,1,6)
  colweights<-rep(1,length(row))
  ans<-maxHist(row, colweights)
  testthat::expect_equal(ans, list(x0=3L, x1=5L, area=12))
})
