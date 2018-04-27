library("distanceR")
context("distances")


test_that("Test distance.euclidean", {
  expect_identical(distance.euclidean(0, 0), 0);
  expect_identical(distance.euclidean(1, 1), 0);
  expect_identical(distance.euclidean(0, 1), 1);
  expect_identical(distance.euclidean(1, 0), 1);
  expect_identical(distance.euclidean(1, -1), 2);
  expect_identical(distance.euclidean(1, Inf), Inf);
  expect_identical(distance.euclidean(Inf, 1), Inf);
  expect_identical(distance.euclidean(Inf, Inf), Inf);
  expect_identical(distance.euclidean(1, -Inf), Inf);
  expect_identical(distance.euclidean(-Inf, 1), Inf);
  expect_identical(distance.euclidean(-Inf, -Inf), Inf);
  expect_identical(distance.euclidean(c(0, 0), c(0, 1)), 1);
  expect_identical(distance.euclidean(c(0, 0), c(1, 0)), 1);
  expect_identical(distance.euclidean(c(0, 1), c(0, 1)), 0);
  expect_identical(distance.euclidean(c(1, 0), c(0, 1)), sqrt(2));
  expect_identical(distance.euclidean(c(1, 1), c(0, 0)), sqrt(2));
  expect_identical(distance.euclidean(c(1, -1), c(0, 0)), sqrt(2));
  expect_identical(distance.euclidean(c(1, 1), c(-1, -1)), sqrt(8));
})
