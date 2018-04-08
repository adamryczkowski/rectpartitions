context("Test of matrix compression")

test_that("Edge cases", {
  rect<-1L
  dim(rect)<-c(1,1)
  ans<-compress(rect)
  testthat::expect_equal(as.integer(ans), as.integer(rect))
  ans<-uncompress(ans)
  testthat::expect_equal(ans, rect)

  rect<-0L
  dim(rect)<-c(1,1)
  ans<-compress(rect)
  testthat::expect_equal(as.integer(ans), as.integer(rect))
  ans<-uncompress(ans)
  testthat::expect_equal(ans, rect)

  rect<-c(1,1)
  dim(rect)<-c(2,1)
  ans<-compress(rect)
  testthat::expect_equal(dim(ans), c(1,1) )
  ans2<-uncompress(ans)
  testthat::expect_equal(ans2, rect)

  rect<-c(1,1)
  dim(rect)<-c(1,2)
  ans<-compress(rect)
  testthat::expect_equal(dim(ans), c(1,1) )
  ans2<-uncompress(ans)
  testthat::expect_equal(ans2, rect)

  rect<-c(1,1,1,1)
  dim(rect)<-c(2,2)
  ans<-compress(rect)
  testthat::expect_equal(dim(ans), c(1,1) )
  ans2<-uncompress(ans)
  testthat::expect_equal(ans2, rect)
})


test_that("Complicated case", {
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
  ans<-compress(rect)
  testthat::expect_equal(dim(ans), c(12,12) )
  ans2<-uncompress(ans)
  testthat::expect_equal(ans2, rect)

})
