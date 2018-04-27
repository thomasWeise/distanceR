library("distanceR")
context("dist.apply")

FUN <- function(i,j) i*1000+j;

tester <- function(n) {
  dm <- dist.apply(n=n, FUN=FUN);
  expect_length(dm, dist.slots(n));
  index <- 0L;
  for(ij in dist.indexes(n=n)) {
    index <- index + 1L;
    expect_identical(dm[index], FUN(ij[1], ij[2]));
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
