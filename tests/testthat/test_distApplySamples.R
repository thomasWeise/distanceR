library("distanceR")
context("dist.apply.samples")

slow.tests <- is.na(Sys.getenv("TRAVIS", unset=NA))

tester.1 <- function(n) {
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



tester.2 <- function(n) {
  data <- lapply(1:n, FUN=function(i) runif(20));

  dm <- dist.apply.samples(data, aggregate = median);
  expect_length(dm, dist.slots(n));

  index <- 0L;
  for(ij in dist.indexes(n=n)) {
    index <- index + 1L;

    a <- ij[1];
    b <- ij[2];

    v <- rep(0, a*b);
    k <- 0;
    for(i in 1:20) {
      for(j in 1:20) {
        k <- 1+k;
        v[k] <- distance.euclidean(data[[a]][i], data[[b]][j]);
      }
    }

    expect_identical(dm[index], median(v));
  }

  expect_identical(dm, dist.apply.samples(data, cores=2L, aggregate = median));
  if(slow.tests) {
    expect_identical(dm, dist.apply.samples(data, cores=3L, aggregate = median));
    expect_identical(dm, dist.apply.samples(data, cores=4L, aggregate = median));
    expect_identical(dm, dist.apply.samples(data, cores=5L, aggregate = median));
    expect_identical(dm, dist.apply.samples(data, cores=6L, aggregate = median));
    expect_identical(dm, dist.apply.samples(data, cores=7L, aggregate = median));
    expect_identical(dm, dist.apply.samples(data, cores=8L, aggregate = median));
    expect_identical(dm, dist.apply.samples(data, cores=9L, aggregate = median));
    expect_identical(dm, dist.apply.samples(data, cores=10L, aggregate = median));
  }
}




test_that("Test dist.apply.samples.1 n=2", {
  tester.1(2);
})

test_that("Test dist.apply.samples.1 n=3", {
  tester.1(3);
})

test_that("Test dist.apply.samples.1 n=4", {
  tester.1(4);
})

test_that("Test dist.apply.samples.1 n=5", {
  tester.1(5);
})

test_that("Test dist.apply.samples.1 n=6", {
  tester.1(6);
})

test_that("Test dist.apply.samples.1 n=7", {
  tester.1(7);
})




test_that("Test dist.apply.samples.2 n=2", {
  tester.2(2);
})

test_that("Test dist.apply.samples.2 n=3", {
  tester.2(3);
})

test_that("Test dist.apply.samples.2 n=4", {
  tester.2(4);
})

test_that("Test dist.apply.samples.2 n=5", {
  tester.2(5);
})

test_that("Test dist.apply.samples.2 n=6", {
  tester.2(6);
})

test_that("Test dist.apply.samples.2 n=7", {
  tester.2(7);
})
