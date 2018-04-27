#' @title Fill Vector with Values for a Distance Matrix
#' @description Create a vector that can be used as backing store for a distance
#'   matrix by applying a function \code{FUN} to all indices that arise when
#'   comparing all the \code{n} objects.
#' @param n the number of objects to compare
#' @param FUN the function to apply to all index pairs \code{i}, \code{j} of
#'   objects to compare
#' @return a vector of length \code{n(n-1)/2} with the results of \code{FUN}
#' @export dist.apply.n
dist.apply.n <- function(n, FUN) {
  stopifnot(n > 1L);
  return(dist.apply(X=1:n, FUN=FUN));
}

#' @title Fill Vector with Values for a Distance Matrix
#' @description Create a vector that can be used as backing store for a distance
#'   matrix by applying a function \code{FUN} to all pairings that arise when
#'   comparing all the objects in \code{X}.
#' @param X objects to compare
#' @param FUN the function to apply to all pairs of elements \code{a}, \code{b}
#'   from \code{X}
#' @return a vector of length \code{n(n-1)/2} with the results of \code{FUN}
#' @export dist.apply
#' @include indexing.R
#' @include distances.R
dist.apply <- function(X, FUN=distance.euclidean) {
  n <- length(X);
  stopifnot(n > 1L);
  res <- vector(mode="numeric", length=dist.slots(n));
  index <- 0L;
  for(i in seq_len(n-1L)) {
    x <- X[[i]];
    for(j in seq.int(from=(i+1L), to=n, by=1L)) {
      index <- (index + 1L);
      res[index] <- FUN(x, X[[j]]);
    }
  }
  stopifnot(identical(index, length(res)));
  return(res);
}
