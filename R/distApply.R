#' @title Fill Vector with Values for a Distance Matrix
#' @description Create a vector that can be used as backing store for a distance
#'   matrix by applying a function \code{FUN} to all indices that arise when
#'   comparing all the \code{n} objects.
#' @param n the number of objects to compare
#' @param FUN the function to apply to all index pairs \code{i}, \code{j} of
#'   objects to compare
#' @param  cores the number of cores to be used for parallel computation
#' @return a vector of length \code{n(n-1)/2} with the results of \code{FUN}
#' @export dist.apply.n
dist.apply.n <- function(n, FUN, cores=1L) {
  stopifnot(n > 1L);
  return(dist.apply(X=1:n, FUN=FUN, cores=cores));
}

#' @title Fill Vector with Values for a Distance Matrix
#' @description Create a vector that can be used as backing store for a distance
#'   matrix by applying a function \code{FUN} to all pairings that arise when
#'   comparing all the objects in \code{X}.
#' @param X objects to compare
#' @param FUN the function to apply to all pairs of elements \code{a}, \code{b}
#'   from \code{X}
#' @param  cores the number of cores to be used for parallel computation
#' @return a vector of length \code{n(n-1)/2} with the results of \code{FUN}
#' @export dist.apply
#' @include indexing.R
#' @include distances.R
#' @importFrom parallel mclapply
dist.apply <- function(X, FUN=distance.euclidean, cores=1L) {
  n <- length(X);
  stopifnot(n > 1L);

  # the required length
  len <- dist.slots(n);

  # cores <= 1: sequential method
  if(cores <= 1L) {
    # allocate destination vector
    res <- vector(mode="numeric", length=len);
    index <- 0L;
    # apply function to all pairs of indexes
    for(i in seq_len(n-1L)) {
      x <- X[[i]];
      for(j in seq.int(from=(i+1L), to=n, by=1L)) {
        index <- (index + 1L);
        res[index] <- FUN(x, X[[j]]);
      }
    }
    # final sanity check
    stopifnot(identical(index, len));
    return(res);
  }

  # ok, cores > 1
  res <- as.vector(unname(unlist(
            mclapply(X=seq_len(n-1L),
               FUN=function(i) {
                 return(vapply(
                        X=seq.int(from=(i+1L), to=n, by=1L),
                          FUN=function(j,x) FUN(x, X[[j]]),
                          FUN.VALUE = +Inf,
                          x=X[[i]]));
               },
               mc.cores=cores,
               mc.preschedule = FALSE),
               recursive = TRUE,
               use.names = FALSE)));

  # final sanity check
  stopifnot(identical(length(res), len));
  return(res);
}
