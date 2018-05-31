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
#' @param  logging the logging setup, see \code{\link[utilizeR]{makeLogger}}
#' @return a vector of values that can be used to produce a distance matrix
#' @include indexing.R
#' @include distApply.R
#' @include ranker.R
#' @export dist.apply.samples.ranked
#' @importFrom utilizeR makeLogger function.name
dist.apply.samples.ranked <- function(X, FUN=distance.euclidean,
                                      sampler=identity,
                                      rank.all=rank.dist,
                                      rank.fromSingle=identity,
                                      aggregate=mean,
                                      FUN.VALUE=+Inf,
                                      cores=1L,
                                      logging=FALSE) {
  n <- length(X);
  stopifnot(n > 1L);
  logging <- makeLogger(logging, cores);

  # extract and cache the samples
  X <- lapply(X=X, FUN=sampler);
  stopifnot(identical(n, length(X)));

  # get the sizes and group indexes
  sizes     <- vapply(X=X, FUN=length, FUN.VALUE=0L);
  stopifnot(identical(length(sizes), n));
  totalSize <- sum(sizes);
  if(!is.null(logging)) {
    logging("Computing distance matrix based on ranking sample distances, for ",
            totalSize, " samples in total using, with",
            (if(identical(rank.all, identity)) {
              if(identical(rank.fromSingle, identity)) {
                "out any ranking"
              } else {
                " local ranking"
              }
            } else {
              if(identical(rank.fromSingle, identity)) {
                " global ranking"
              } else {
                " global and local ranking"
              }
            }),
            ", distance function ", function.name(FUN),
            " and aggregate ", function.name(aggregate), ".");
  }

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
  if(!is.null(logging)) {
    logging("Now computing a big distance matrix with the distances between all samples.");
  }
  dm <- rank.all(dist.apply.n(n=totalSize, FUN=fun, cores=cores, logging=logging));
  if(!is.null(logging)) {
    logging("Done computing the big distance matrix, now computing the rankings.");
  }
  X <- NULL;

  # create the distances storage: for a pair i, j, we need 2*sample_count(i) * sample_count(j) entries
  results.sizes <- dist.apply.samples(X=sizes, FUN=function(i,j) 2*i*j);
  results <- lapply(X=results.sizes,
                    FUN=function(i) vector(mode="numeric", length=i));

  # now we compute each and every distance
  computed <- NULL;
  for(i in seq_len(totalSize)) {
    # get the group of the first sample
    i.g <- groups[i];

    # allocate the vector if necessary
    n.cur <- (totalSize-sizes[[i.g]]);
    if(!identical(n.cur, length(computed))) {
      computed <- vector(mode="numeric", length=n.cur);
    }

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
      j.g <- groups[j];
      if(!(identical(i.g, j.g))) {
        index <- index + 1L;
        didx <- dist.index(i.g, j.g, n);
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

  if(!is.null(logging)) {
    logging("Finished computing distance samples, now collapsing distances using aggregate.");
  }

  # collapse the results into a distance matrix data list
  ret <- vapply(X=results, FUN=function(x) { if(length(x)>0L) aggregate(x) else FUN.VALUE },
                FUN.VALUE=FUN.VALUE);
  if(!is.null(logging)) {
    logging("Done computing distance matrix.");
  }
  return(ret);
}
