library("distanceR")
context("merge")

test_that("Test merge 1", {
  dm1 <- dist.create(
    distances=dist.apply.n(4, FUN=function(i, j) abs(i-j)),
    names=c("a", "b", "c", "d")
  );
  expect_length(dm1, 6);

  dm2 <- dist.merge(dm1, list(c(1, 2), c(3, 4)), names=c("ab", "cd"));

  expect_length(dm2, 1);
  expect_equal(dist.get(dm2, 1, 2),
               mean(c(dist.get(dm1, 1, 3),
                      dist.get(dm1, 1, 4),
                      dist.get(dm1, 2, 3),
                      dist.get(dm1, 2, 4))));

  dm2 <- dist.merge(dm1, list(c(1, 2), 3, 4), names=c("ab", "c", "d"));
  expect_length(dm2, 3);
  expect_equal(dist.get(dm2, 1, 2),
               mean(c(dist.get(dm1, 1, 3),
                      dist.get(dm1, 2, 3))));
  expect_equal(dist.get(dm2, 1, 3),
               mean(c(dist.get(dm1, 1, 4),
                      dist.get(dm1, 2, 4))));
  expect_equal(dist.get(dm2, 2, 3),
               dist.get(dm1, 3, 4));

  dm2 <- dist.merge(dm1, list(1, 2, 3, 4), names=c("a", "b", "c", "d"));
  expect_equal(dm2, dm1);

  dm2 <- dist.merge(dm1, list(1, c(2, 3, 4)), names=c("a", "bcd"));
  expect_length(dm2, 1);
  expect_equal(dist.get(dm2, 1, 2),
               mean(c(dist.get(dm1, 1, 2),
                      dist.get(dm1, 1, 3),
                      dist.get(dm1, 1, 4) )));
})


test_that("Test merge 2", {
  dm1 <- dist.create(
    distances=dist.apply.n(6, FUN=function(i, j) runif(n=1)),
    names=c("a", "b", "c", "d", "e", "f")
  );
  expect_length(dm1, 15);

  dm2 <- dist.merge(dm1, list(c(1, 2), c(3, 4, 5), c(6)), names=c("ab", "cde", "f"));
  expect_length(dm2, 3);
  expect_equal(dist.get(dm2, 1, 2),
               mean(c(dist.get(dm1, 1, 3),
                      dist.get(dm1, 1, 4),
                      dist.get(dm1, 1, 5),
                      dist.get(dm1, 2, 3),
                      dist.get(dm1, 2, 4),
                      dist.get(dm1, 2, 5))));
  expect_equal(dist.get(dm2, 1, 3),
               mean(c(dist.get(dm1, 1, 6),
                      dist.get(dm1, 2, 6))));
  expect_equal(dist.get(dm2, 2, 3),
               mean(c(dist.get(dm1, 3, 6),
                      dist.get(dm1, 4, 6),
                      dist.get(dm1, 5, 6))));
})


test_that("Test merge 3", {
  dm1 <- dist.create(
    distances=dist.apply.n(3, FUN=function(i, j) rnorm(n=1)),
    names=c("a", "b", "c")
  );
  expect_length(dm1, 3);

  dm2 <- dist.merge(dm1, list(c(1, 2), 3), names=c("ab", "c"));
  expect_length(dm2, 1);
  expect_equal(dist.get(dm2, 1, 2),
               mean(c(dist.get(dm1, 1, 3),
                      dist.get(dm1, 2, 3))));

  dm2 <- dist.merge(dm1, list(c(3, 2), 1), names=c("ba", "c"));
  expect_length(dm2, 1);
  expect_equal(dist.get(dm2, 1, 2),
               mean(c(dist.get(dm1, 1, 3),
                      dist.get(dm1, 1, 2))));

  dm2 <- dist.merge(dm1, list(3, c(1, 2)), names=c("c", "ab"));
  expect_length(dm2, 1);
  expect_equal(dist.get(dm2, 1, 2),
               mean(c(dist.get(dm1, 1, 3),
                      dist.get(dm1, 2, 3))));

  dm2 <- dist.merge(dm1, list(1, c(2, 3)), names=c("c", "ba"));
  expect_length(dm2, 1);
  expect_equal(dist.get(dm2, 1, 2),
               mean(c(dist.get(dm1, 1, 3),
                      dist.get(dm1, 1, 2))));


  dm2 <- dist.merge(dm1, list(1, c(2, 3)), names=c("a", "bc"));
  expect_length(dm2, 1);
  expect_equal(dist.get(dm2, 1, 2),
               mean(c(dist.get(dm1, 1, 2),
                      dist.get(dm1, 1, 3))));

  dm2 <- dist.merge(dm1, list(1, c(3, 2)), names=c("a", "cb"));
  expect_length(dm2, 1);
  expect_equal(dist.get(dm2, 1, 2),
               mean(c(dist.get(dm1, 1, 2),
                      dist.get(dm1, 1, 3))));


  dm2 <- dist.merge(dm1, list(c(2, 3), 1), names=c("bc", "a"));
  expect_length(dm2, 1);
  expect_equal(dist.get(dm2, 1, 2),
               mean(c(dist.get(dm1, 1, 2),
                      dist.get(dm1, 1, 3))));

  dm2 <- dist.merge(dm1, list(c(3, 2), 1), names=c("cb", "a"));
  expect_length(dm2, 1);
  expect_equal(dist.get(dm2, 1, 2),
               mean(c(dist.get(dm1, 1, 2),
                      dist.get(dm1, 1, 3))));


  dm2 <- dist.merge(dm1, list(c(1, 3), 2), names=c("ac", "b"));
  expect_length(dm2, 1);
  expect_equal(dist.get(dm2, 1, 2),
               mean(c(dist.get(dm1, 1, 2),
                      dist.get(dm1, 3, 2))));

  dm2 <- dist.merge(dm1, list(c(3, 1), 2), names=c("ca", "b"));
  expect_length(dm2, 1);
  expect_equal(dist.get(dm2, 1, 2),
               mean(c(dist.get(dm1, 1, 2),
                      dist.get(dm1, 3, 2))));
})
