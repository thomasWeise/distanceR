library("distanceR")
context("connections")

test_that("Test connections 1", {
  dm <- dist.create(dist.apply(X=c(1, 2, 4, 3, 10, 11)));
  con <- dist.connections(dm, 1);
  expect_identical(con,
                   list(c(1L, 2L),
                        c(2L, 4L),
                        c(3L, 4L),
                        c(5L, 6L)));
})
