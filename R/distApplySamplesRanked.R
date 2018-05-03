#' @title Create a Distance Vector based on Normalized Ranking of Distances
#' @description Create a distance matrix/vector where the actual distances from
#'   samples are replaced by their mean ranks. This should allow for
#'   scale-independent, robust distance matrices.
#' @param X the list or vector of elements to be compared
#' @param FUN the distance function, a function accepting two \emph{samples} and
#'   returning one distance value
#' @param sampler the sampling function, returning a vector or list of samples
#'   for an element of \code{X}
#' @param rank.all the ranking to be applied to all distances
#' @param rank.fromSingle the ranking to be applied to all the distances from
#'   one specific sample to the other samples
#' @param aggregate the aggregation function which will join distances computed
#'   with \code{FUN} of all combinations of the samples from two elements of
#'   \code{X} into a single value
#' @param FUN.VALUE the value to be used for situations where an element of
#'   \code{X} contains no samples
#' @param  cores the number of cores to be used for parallel computation
#' @return a vector of values that can be used to produce a distance matrix
#' @include indexing.R
#' @include distApply.R
#' @include ranker.R
#' @export dist.apply.samples.ranked
dist.apply.samples.ranked <- function(X, FUN=distance.euclidean,
                                      sampler=identity,
                                      rank.all=rank.dist,
                                      rank.fromSingle=identity,
                                      aggregate=mean,
                                      FUN.VALUE=+Inf,
                                      cores=1L) {
  n <- length(X);
  stopifnot(n > 1L);

  # get the sizes and group indexes
  sizes     <- vapply(X=X, FUN=length, FUN.VALUE=0L);
  stopifnot(identical(length(sizes), n));
  totalSize <- sum(sizes);

  # get the group indexes
  groups    <- unlist(lapply(X=seq_len(n), FUN=function(i) rep(x=i, times=sizes[[i]])),
                      recursive=TRUE, use.names=FALSE);
  stopifnot(identical(length(groups), totalSize));

  # flatten the list of samples
  X <- unlist(X, recursive = FALSE, use.names = FALSE);
  stopifnot(identical(length(X), totalSize));

  # wrap the distance function such that distances are only computed for
  # elements in different groups and distances for elements in the same group
  # are considered to be NA
  fun <- function(i, j) if(identical(groups[i], groups[j])) NA else FUN(X[[i]], X[[j]]);

  # compute all the single distances using the wrapped distance function and
  # build the huge distance matrix, with NA for intra-group distances - this can
  # be done in parallel
  dm <- rank.all(dist.apply.n(n=length(X), FUN=fun, cores=cores));
  X <- NULL;

  # create the distances storage: for a pair i, j, we need sample_count(i) + sample_count(j) entries
  results.sizes <- dist.apply.samples(X=sizes, FUN=`+`);
  results <- lapply(X=results.sizes,
                    FUN=function(i) vector(mode="numeric", length=i));

  # now we compute each and every distance
  computed <- NULL;
  for(i in seq_len(totalSize)) {
    # allocate the vector if necessary
    n.cur <- (totalSize-sizes[[i]]);
    if(!identical(n.cur, length(computed))) {
      computed <- vector(mode="numeric", length=n.cur);
    }

    # get the group of the first sample
    i.g <- groups[i];

    # compute all distances
    index <- 0L;
    for(j in seq_len(totalSize)) {
      if(!(identical(i.g, groups[j]))) {
        index <- index + 1L;
        computed[index] <- dist.get(dist=dm, i=i, j=j, n=totalSize);
      }
    }
    stopifnot(identical(index, n.cur));

    # rank the distances
    ranks <- rank.fromSingle(computed);

    # store back the results
    index <- 0L;
    for(j in seq_len(totalSize)) {

      if(!(identical(i.g, groups[j]))) {
        index <- index + 1L;
        didx <- dist.index(i, j, n);
        s <- results.sizes[[didx]];
        results.sizes[[didx]] <- (s - 1L);
        results[[didx]][s] <- ranks[[index]];
      }
    }
  }
  stopifnot(all(results.sizes == 0L));

  # dispose all no-longer needed data
  dm <- NULL;
  sizes <- NULL;
  groups <- NULL;
  ranks <- NULL;
  computed <- NULL;

  # collapse the results into a distance matrix data list
  return(vapply(X=results, FUN=function(x) { if(length(x)>0L) aggregate(x) else FUN.VALUE },
                FUN.VALUE=FUN.VALUE));
}
