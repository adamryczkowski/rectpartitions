context("Tests stack")

test_that("Is empty", {
  stack<-new.env()
  testthat::expect_true(stack.is_empty(stack))
  testthat::expect_equal(stack.length(stack),0)
})

test_that("Is non-empty", {
  stack<-new.env()
  stack.push(stack, 1)
  testthat::expect_equal(stack.length(stack),1)
  testthat::expect_false(stack.is_empty(stack))
})

test_that("push & pop", {
  stack<-new.env()
  stack.push(stack, 1)
  testthat::expect_equal(stack.top(stack), 1)
  testthat::expect_false(stack.is_empty(stack))
  testthat::expect_equal(stack.pop(stack), 1)
  testthat::expect_true(stack.is_empty(stack))
})

test_that("push & pop & push & pop", {
  stack<-new.env()
  stack.push(stack, 1)
  stack.pop(stack)
  stack.push(stack, 2)
  testthat::expect_equal(stack.pop(stack), 2)
  testthat::expect_true(stack.is_empty(stack))
})

test_that("push & push & pop & pop", {
  stack<-new.env()
  stack.push(stack, 1)
  stack.push(stack, 2)
  testthat::expect_equal(stack.length(stack),2)
  stack.pop(stack)
  testthat::expect_equal(stack.length(stack),1)
  testthat::expect_false(stack.is_empty(stack))
  testthat::expect_equal(stack.pop(stack), 1)
  testthat::expect_true(stack.is_empty(stack))
  testthat::expect_equal(stack.length(stack),0)

})

test_that("push & push & push & pop & push & pop & pop & pop ", {
  stack<-new.env()
  stack.push(stack, 1)
  stack.push(stack, 2)
  stack.push(stack, 3)
  testthat::expect_equal(stack.length(stack),3)
  testthat::expect_equal(stack.top(stack), 3)
  stack.pop(stack)
  testthat::expect_equal(stack.top(stack), 2)
  stack.push(stack, 4)
  l<-unlist(stack.to_list(stack))
  testthat::expect_equal(l, c(1,2,4))
  testthat::expect_equal(stack.length(stack),3)
  testthat::expect_equal(stack.top(stack), 4)
  stack.pop(stack)
  stack.pop(stack)
  stack.pop(stack)
  testthat::expect_true(stack.is_empty(stack))
})
