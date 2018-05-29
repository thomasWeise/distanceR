#' @title Build Distance Matrix Data based on Per-Index Sampling
#' @description Build the data for a distance matrix by comparing the elements
#'   from a vector or list \code{X}. Here, \code{X} can be a list of lists or a
#'   list of objects. Each element of \code{X} is passed to a sampler function
#'   \code{sampler} which will return the "samples" for that element. These
#'   samples are the actual values passed to the distance function \code{FUN}:
#'   If we want to compute the distance between two elements \code{a} and
#'   \code{b} from \code{X}, then what we do is first computing
#'   \code{sa=sampler(a)} and \code{sb=sampler(b)}. \code{sa} and \code{sb}
#'   should be lists or vectors. We then go through all combinations of the
#'   elements in \code{sa} and \code{sb} and pass them to \code{FUN}, one after
#'   the other. The results of these \code{length(sa) * length(sb)} computations
#'   are finally passed to \code{aggregate} as vector. The result of
#'   \code{aggregate} is then the distance between \code{a} and \code{b}.
#' @param X the list or vector of elements to be compared
#' @param FUN the distance function, a function accepting two \emph{samples} and
#'   returning one distance value
#' @param sampler the sampling function, returning a vector or list of samples
#'   for an element of \code{X}
#' @param aggregate the aggregation function which will join distances computed
#'   with \code{FUN} of all combinations of the samples from two elements of
#'   \code{X} into a single value
#' @param FUN.VALUE the value to be used for situations where an element of
#'   \code{X} contains no samples
#' @param  cores the number of cores to be used for parallel computation
#' @param  logging the logging setup, see \code{\link[utilizeR]{makeLogger}}
#' @return a vector of values that can be used to produce a distance matrix
#' @export dist.apply.samples
#' @include distApply.R
#' @include distances.R
#' @seealso dist.apply
#' @seealso dist.create
#' @importFrom utilizeR makeLogger
dist.apply.samples <- function(X, FUN=distance.euclidean,
                               sampler=identity, aggregate=mean,
                               FUN.VALUE=+Inf,
                               cores=1L,
                               logging=FALSE) {
  logging <- makeLogger(logging, cores);
  if(!is.null(logging)) {
    logging("Computing sample-based distances using aggregate ",
            as.character(substitute(aggregate)), ".");
  }

  ret <- dist.apply(X=X, FUN=function(a, b) {
    a.samples <- sampler(a);
    a.length  <- length(a.samples);
    if(a.length <= 0L) { return(FUN.VALUE); }
    b.samples <- sampler(b);
    b.length  <- length(b.samples);
    if(b.length <= 0L) { return(FUN.VALUE); }
    return(aggregate(
      vapply(X=seq.int(from=0L, to=(a.length * b.length - 1L), by=1L),
             FUN=function(z)
               FUN(a.samples[[1L + (z %/% b.length)]],
                   b.samples[[1L + (z %%  b.length)]]),
               FUN.VALUE = FUN.VALUE)
    ))
  }, cores=cores, logging=logging);

  if(!is.null(logging)) {
    logging("Finished computing sample-based distances.");
  }
  return(ret);
}
