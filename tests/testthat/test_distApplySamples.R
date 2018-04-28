library("distanceR")
context("dist.apply.samples")

slow.tests <- is.na(Sys.getenv("TRAVIS", unset=NA))

tester <- function(n) {
  data <- lapply(1:n, FUN=function(i) vapply(X=1:i, FUN=identity, FUN.VALUE=-Inf));

  dm <- dist.apply.samples(data);
  expect_length(dm, dist.slots(n));

  index <- 0L;
  for(ij in dist.indexes(n=n)) {
    index <- index + 1L;

    a <- ij[1];
    b <- ij[2];

    v <- rep(0, a*b);
    k <- 0;
    for(i in 1:a) {
      for(j in 1:b) {
        k <- 1+k;
        v[k] <- distance.euclidean(i, j);
      }
    }

    expect_identical(dm[index], mean(v));
  }

  expect_identical(dm, dist.apply.samples(data, cores=2L));
  if(slow.tests) {
    expect_identical(dm, dist.apply.samples(data, cores=3L));
    expect_identical(dm, dist.apply.samples(data, cores=4L));
    expect_identical(dm, dist.apply.samples(data, cores=5L));
    expect_identical(dm, dist.apply.samples(data, cores=6L));
    expect_identical(dm, dist.apply.samples(data, cores=7L));
    expect_identical(dm, dist.apply.samples(data, cores=8L));
    expect_identical(dm, dist.apply.samples(data, cores=9L));
    expect_identical(dm, dist.apply.samples(data, cores=10L));
  }
}

test_that("Test dist.apply.samples n=2", {
  tester(2);
})

test_that("Test dist.apply.samples n=3", {
  tester(3);
})

test_that("Test dist.apply.samples n=4", {
  tester(4);
})

test_that("Test dist.apply.samples n=5", {
  tester(5);
})

test_that("Test dist.apply.samples n=6", {
  tester(6);
})

test_that("Test dist.apply.samples n=7", {
  tester(7);
})
