library("distanceR")
context("create")

test_that("Test create 1", {
  dists <- vapply(X=dist.indexes(2),
                  FUN=function(id) (id[1] - id[2])^2,
                  FUN.VALUE = NaN)
  dm <- dist.create(dists, c("A", "B"));
  expect_is(dm, "dist");
  hclust(dm)
})

test_that("Test create 2", {
  dists <- vapply(X=dist.indexes(5),
                  FUN=function(id) (id[1] - id[2])^2,
                  FUN.VALUE = NaN)
  dm <- dist.create(dists, c("A", "B", "C", "D", "E"));
  expect_is(dm, "dist");
  hclust(dm)
})

test_that("Test create 2", {
  expect_error(dist.create(NULL, NULL));
  expect_error(dist.create(NULL, c("a", "b")));
  expect_error(dist.create(c(1), NULL));
  expect_error(dist.create(c(1), c("a", "b", "c")));
  expect_error(dist.create(c(1, 3), c("a", "b", "c")));
})
