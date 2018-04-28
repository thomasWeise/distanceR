library("distanceR")
context("dist.apply")

FUN <- function(i,j) i*1000+j;
slow.tests <- is.na(Sys.getenv("TRAVIS", unset=NA))

tester <- function(n) {
  dm <- dist.apply(1:n, FUN=FUN);
  expect_length(dm, dist.slots(n));
  index <- 0L;
  for(ij in dist.indexes(n=n)) {
    index <- index + 1L;
    expect_identical(dm[index], FUN(ij[1], ij[2]));
  }

  expect_identical(dm, dist.apply(1:n, FUN=FUN, cores=2L));
  if(slow.tests) {
    expect_identical(dm, dist.apply(1:n, FUN=FUN, cores=3L));
    expect_identical(dm, dist.apply(1:n, FUN=FUN, cores=4L));
    expect_identical(dm, dist.apply(1:n, FUN=FUN, cores=5L));
    expect_identical(dm, dist.apply(1:n, FUN=FUN, cores=6L));
    expect_identical(dm, dist.apply(1:n, FUN=FUN, cores=7L));
    expect_identical(dm, dist.apply(1:n, FUN=FUN, cores=8L));
  }
}

test_that("Test dist.apply n=2", {
  tester(2);
})

test_that("Test dist.apply n=3", {
  tester(3);
})

test_that("Test dist.apply n=4", {
  tester(4);
})

test_that("Test dist.apply n=5", {
  tester(5);
})

test_that("Test dist.apply n=6", {
  tester(6);
})

test_that("Test dist.apply n=7", {
  tester(7);
})
