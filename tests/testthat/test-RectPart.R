context("Maximum size rectangle problem")

test_that("Simple edge cases", {
  rect<-0
  dim(rect)<-c(1,1)
  ans<-maxRectangle(rect, 1, 1)
  testthat::expect_equal(ans$area, 0)

  rect<-1
  dim(rect)<-c(1,1)
  ans<-maxRectangle(rect, 1, 1)
  testthat::expect_equal(ans$area, 1)

  rect<-c(0,0)
  dim(rect)<-c(2,1)
  ans<-maxRectangle(rect, colweights = 1, rowweights = c(1,1))
  testthat::expect_equal(ans$area, 0)

  rect<-c(0,0)
  dim(rect)<-c(1,2)
  ans<-maxRectangle(rect, colweights = c(1,1), rowweights = c(1))
  testthat::expect_equal(ans$area, 0)

  rect<-c(0,1)
  dim(rect)<-c(2,1)
  ans<-maxRectangle(rect, colweights = 1, rowweights = c(1,1))
  testthat::expect_equal(ans$area, 1)

  rect<-c(0,1)
  dim(rect)<-c(1,2)
  ans<-maxRectangle(rect, colweights = c(1,2), rowweights = c(1))
  testthat::expect_equal(ans$area, 2)

  rect<-c(0,1)
  dim(rect)<-c(2,1)
  ans<-maxRectangle(rect, colweights = c(1), rowweights = c(1,2))
  testthat::expect_equal(ans$area, 2)

  rect<-c(1,1)
  dim(rect)<-c(2,1)
  ans<-maxRectangle(rect, colweights = c(1), rowweights = c(1,2))
  testthat::expect_equal(ans$area, 3)

  rect<-c(0,0,1,0)
  dim(rect)<-c(2,2)
  ans<-maxRectangle(rect, colweights = c(1,3), rowweights = c(1,2))
  testthat::expect_equal(ans$area, 3)

  rect<-c(1,1,1,1)
  dim(rect)<-c(2,2)
  ans<-maxRectangle(rect, colweights = c(1,3), rowweights = c(1,2))
  testthat::expect_equal(ans$area, 12)
})

test_that("Example from https://www.geeksforgeeks.org/maximum-size-rectangle-binary-sub-matrix-1s/", {
  rect<-as.integer(c(0, 1, 1, 0,
                     1, 1, 1, 1,
                     1, 1, 1, 1,
                     1, 1, 0, 0))
  dim(rect)<-c(4,4)
  ans<-maxRectangle(rect, colweights = rep(1,4), rowweights = rep(1,4))
  testthat::expect_equal(ans, list(x0=2L, x1=3L, area=8, y0=1L, y1=4L))
}

test_that("Bigger test", {

  rect<-as.integer(c(
    0,0,0,0,0,0,0,0,0,1,1,1,1,1,1,1,0,
    0,0,0,0,1,1,1,1,0,1,1,1,1,1,1,1,0,
    0,0,0,0,0,1,0,0,0,1,1,1,1,1,1,1,0,
    0,0,0,0,1,1,0,0,0,1,1,1,1,1,1,1,0,
    0,0,0,0,1,1,1,1,0,0,0,0,1,1,0,0,0,
    0,0,0,0,1,1,1,1,0,1,0,0,1,1,1,0,0,
    0,0,0,0,0,1,1,1,0,1,1,1,0,0,1,1,0,
    0,0,0,0,0,0,1,1,0,1,1,1,0,0,1,1,0,
    0,0,0,0,1,1,1,1,0,0,1,1,0,0,1,1,0,
    0,0,0,0,0,1,1,1,0,0,0,0,0,0,1,1,0,
    0,0,0,0,0,0,0,1,0,0,0,0,0,0,1,0,0,
    0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0))
  dim(rect)<-c(17,12)
  rect<-t(rect)

  ans<-maxRectangle(rect, colweights = rep(1,ncol(rect)), rowweights = rep(1,nrow(rect)))

  expect_equal(ans, list(x0=10L, x1=16L, area=28, y0=1L, y1=4L))
})
