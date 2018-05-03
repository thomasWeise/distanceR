library("distanceR")
context("rank.dist")

test_that("Test rank.dist", {

  expect_equal(rank.dist(c(1e3)), 0.5);
  expect_equal(rank.dist(c(3)), 0.5);
  expect_equal(rank.dist(c(1)), 0.5);
  expect_equal(rank.dist(c(0)), 0);
  expect_equal(rank.dist(c(NA)), 2);

  expect_equal(rank.dist(c(1, 1)),
               c(0.5, 0.5));

  expect_equal(
    rank.dist(c(1, 2, 3)),
    c(1L, 2L, 3L) / 3);
  expect_equal(
    rank.dist(c(3, 2, 1)),
    c(3L, 2L, 1L) / 3);

  expect_equal(
    rank.dist(c(0, 1, 2, 3)),
    c(0L, 1L, 2L, 3L) / 3);
  expect_equal(
    rank.dist(c(0, 2, 1, 3)),
    c(0L, 2L, 1L, 3L) / 3);

  expect_equal(
    rank.dist(c(3, NA, 2, 1)),
    c(1, 2, 2/3, 1/3));
  expect_equal(
    rank.dist(c(3, NA, 2, 1, NA)),
    c(1, 2, 2/3, 1/3, 2));

  expect_equal(
    rank.dist(c(3, NA, 0, 2, 1)),
    c(1, 2, 0, 2/3, 1/3));
  expect_equal(
    rank.dist(c(3, 0, NA, 2, 1, NA)),
    c(1, 0, 2, 2/3, 1/3, 2));

  expect_equal(
    rank.dist(c(0, 3, NA, 0, 2, 1)),
    c(0, 1, 2, 0, 2.5/3.5, 1.5/3.5));
  expect_equal(
    rank.dist(c(3, 0, NA, 0, 2, 0, 1, NA)),
    c(1, 0, 2, 0, 3/4, 0, 2/4, 2));
})
