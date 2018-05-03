library("distanceR")
context("dist.apply.samples.ranked")

slow.tests <- is.na(Sys.getenv("TRAVIS", unset=NA))

tester.1 <- function(n) {
  data <- 1:n;

  dm <- dist.apply.samples.ranked(data);
  expect_length(dm, dist.slots(n));

  dists <- dist.apply(data);
  ranks <- rank(dists);
  ranks <- ranks / max(ranks);

  expect_equal(dm, ranks);

  expect_identical(dm, dist.apply.samples.ranked(data, cores=2L));
  if(slow.tests) {
    expect_identical(dm, dist.apply.samples.ranked(data, cores=3L));
    expect_identical(dm, dist.apply.samples.ranked(data, cores=4L));
    expect_identical(dm, dist.apply.samples.ranked(data, cores=5L));
    expect_identical(dm, dist.apply.samples.ranked(data, cores=6L));
    expect_identical(dm, dist.apply.samples.ranked(data, cores=7L));
    expect_identical(dm, dist.apply.samples.ranked(data, cores=8L));
  }
}

test_that("Test dist.apply.samples.ranked n=2", {
  tester.1(2);
})

test_that("Test dist.apply.samples.ranked n=3", {
  tester.1(3);
})

test_that("Test dist.apply.samples.ranked n=4", {
  tester.1(4);
})

test_that("Test dist.apply.samples.ranked n=5", {
  tester.1(5);
})

test_that("Test dist.apply.samples.ranked n=6", {
  tester.1(6);
})

test_that("Test dist.apply.samples.ranked n=7", {
  tester.1(7);
})
