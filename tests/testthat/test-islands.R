context("Tests partitioning of the matrix into contiguous islands of ones")

test_that("Edge cases", {
  rect<-1L
  dim(rect)<-c(1,1)
  ans<-partition_rect(rect)
  testthat::expect_equal(length(ans),1)
  ans<-uncompress(ans[[1]])
  testthat::expect_equal(ans, rect)

  rect<-0L
  dim(rect)<-c(1,1)
  ans<-partition_rect(rect)
  testthat::expect_equal(length(ans),0)

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
  ans<-partition_rect(rect)
  testthat::expect_equal(length(ans), 3 )
  testthat::expect_equal(sum(purrr::map_dbl(ans, ~sum(uncompress(.)))), sum(rect))

})
