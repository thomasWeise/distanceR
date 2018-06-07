library("distanceR")
context("indexing")


test_that("Test indexing 1", {
  n <- 10;
  test.dist <- dist(1:n);
  test.mat  <- as.matrix(test.dist);

  for(i in 1:n) {
    for(j in 1:n) {
      expect_equal(test.mat[i, j], dist.get(test.dist, i, j, n));
      expect_equal(test.mat[i, j], dist.get(test.dist, j, i, n));
      expect_equal(test.mat[j, i], dist.get(test.dist, i, j, n));
      expect_equal(test.mat[i, j], dist.get(test.dist, j, i, n));
      expect_identical(dist.get(test.dist, i, j, n), dist.get(test.dist, i, j));
      if(i == j) {
        expect_equal(test.mat[j, i], 0);
      }

      index <- dist.index(i, j, n);
      if(index != 0) {
        expect_equal(c(min(i,j), max(i, j)), dist.ij(index, n));
      }
    }
  }
})


test_that("Test indexing 2", {
  n <- 33;

  indexes <- dist.indexes(n);
  for(i in 1:length(indexes)) {
    expect_equal(indexes[[i]], dist.ij(i, n));
    expect_equal(dist.index(indexes[[i]][1], indexes[[i]][2], n), i);
  }
})


test_that("Test dist.n and dist.slots", {

  for(n in 1L:200L) {
    z <- dist.slots(n);
    expect_equal(z, (n*(n-1))/2);
    expect_identical(n, dist.n(z));

    m <- matrix(runif(n=n*5), nrow=n);
    d <- dist(m);
    expect_identical(n, dist.n.from.dm(d));
    v <- as.vector(d);
    expect_identical(n, dist.n.from.dm(v));
  }
})
