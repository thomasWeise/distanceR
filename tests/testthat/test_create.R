library("distanceR")
context("create")


test_that("Test create 1", {
  dists <- vapply(X=dist.indexes(5),
                  FUN=function(id) (id[1] - id[2])^2,
                  FUN.VALUE = NaN)
  dm <- dist.create(dists, c("A", "B", "C", "D", "E"));
  expect_is(dm, "dist");
  hclust(dm)
})
