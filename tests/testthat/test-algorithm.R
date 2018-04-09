context("Tests the whole algorithm")

test_that("Case 1", {
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
  ans1<-get_rectangles(rect)
  debugonce(get_rectangles_shuffle)
  ans2<-get_rectangles_shuffle(rect)
  ans3<-get_rectangles_multishuffle(rect)

  rect<-t(rect)
  ans3<-get_rectangles(rect)
  ans4<-get_rectangles_shuffle(rect)

  testthat::expect_equal(length(ans1), 15 )
  testthat::expect_equal(length(ans3), 15 )
  testthat::expect_equal(length(ans2), 14 )
  testthat::expect_equal(length(ans4), 14 )
})
