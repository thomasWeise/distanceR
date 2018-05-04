library("distanceR")
context("dist.apply.samples.ranked")

slow.tests <- is.na(Sys.getenv("TRAVIS", unset=NA))

tester.2 <- function(n) {
  data <- lapply(X=1:n, FUN=function(i) runif(n=runif(n=1, min=1, max=20)));

  dm <- dist.apply.samples.ranked(data, rank.all=rank.dist, rank.fromSingle=identity);
  expect_length(dm, dist.slots(n));

  infos <- vector(mode="list", length=n);
  start <- 1L;
  for(i in 1:n) {
    end <- start + length(data[[i]]);
    infos[[i]] <- c(start, end - 1L);
    start <- end;
  }

  data2 <- do.call(c, lapply(X=1:n, FUN=function(i) {
                  lapply(X=data[[i]], FUN=function(d) {attr(d, "i") <- i; d }) }));

  dists <- rank.dist(dist.apply(data2, FUN=function(a,b) { if(attr(a, "i") == attr(b, "i")) NA else distance.euclidean(a, b) }));

  dm2 <- dist.apply.samples(X=1:n, FUN=function(i, j) dists[[dist.index(i, j, length(data2))]],
                            sampler=function(i) seq.int(from=infos[[i]][1], to=infos[[i]][2], by=1));

  expect_equal(dm, dm2);

  expect_identical(dm, dist.apply.samples.ranked(data, cores=2L));
  if(slow.tests) {
    expect_identical(dm, dist.apply.samples.ranked(data, rank.all=rank.dist, rank.fromSingle=identity, cores=3L));
    expect_identical(dm, dist.apply.samples.ranked(data, rank.all=rank.dist, rank.fromSingle=identity, cores=4L));
    expect_identical(dm, dist.apply.samples.ranked(data, rank.all=rank.dist, rank.fromSingle=identity, cores=5L));
    expect_identical(dm, dist.apply.samples.ranked(data, rank.all=rank.dist, rank.fromSingle=identity, cores=6L));
    expect_identical(dm, dist.apply.samples.ranked(data, rank.all=rank.dist, rank.fromSingle=identity, cores=7L));
    expect_identical(dm, dist.apply.samples.ranked(data, rank.all=rank.dist, rank.fromSingle=identity, cores=8L));
  }
}

test_that("Test dist.apply.samples.ranked n=2", {
  tester.2(2);
})

test_that("Test dist.apply.samples.ranked n=3", {
  tester.2(3);
})

test_that("Test dist.apply.samples.ranked n=4", {
  tester.2(4);
})

test_that("Test dist.apply.samples.ranked n=5", {
  tester.2(5);
})

test_that("Test dist.apply.samples.ranked n=6", {
  tester.2(6);
})

test_that("Test dist.apply.samples.ranked n=7", {
  tester.2(7);
})
