#' @title Rank a Vector of Distance Values
#' @description This function computes the ranks of a vector of distance values.
#'   The resulting array will contain normalized ranks in \code{[0, 1]}.
#'   \code{NA} distances are allowed and will receive ranks \code{> 1}. If the
#'   smallest non-\code{NA} distance in \code{X} is \code{0}, all such distances
#'   will receive rank \code{0}. The largest non-\code{NA} distance in \code{X}
#'   will receive rank \code{1}. If the smallest non-\code{NA} distance in
#'   \code{X} is larger than zero, it will be assigned a value in \code{(0,1)},
#'   namely its raw rank divided by the maximum rank.
#' @param X the array of distance values
#' @param na.value the value to be used for NA distances. since all "normal"
#'   distances will be normalized into \code{[0, 1]}, a value bigger than that
#'   would make sense, say \code{2} or \code{+Inf}.
#' @return an array with normalized distance ranks
#' @export rank.dist
rank.dist <- function(X, na.value=2) {
  # get the overall length
  n <- length(X);

  # get the locations and number of NA distances
  na    <- is.na(X);
  n.na  <- sum(na);

  if(n.na >= n) {
    # if all values are NA, return a vector of value na.value
    return(rep(x=na.value, times=n));
  }

  # rank all the distances
  ranks <- rank(x=X, na.last=TRUE, ties.method="average");

  # get the range of distances
  range <- range(ranks, na.rm=TRUE);
  # the NA distances will always be at the end, so we can get the maximum non-NA
  # distance rank by subtracting n.na from the maximum rank
  min   <- range[1];
  max   <- range[2] - n.na;

  # get the minimum actual distance value, se we can check if it is <= 0
  X.min <- min(X, na.rm=TRUE);

  if(min >= max) {
    # deal with a situation where all non-NA distances are the same
    if(X.min <= 0) {
      # if this is true, all non-NA distances must be 0
      # so we set the normalized ranks to 0, too
      ranks[ranks <= max] <- 0;
    } else {
      # otherwise, the normalized ranks should be 0.5
      ranks[ranks <= max] <- 0.5;
    }
  } else {
    # ok, the minimum rank is smaller than the maximum rank
    if(X.min <= 0) {
      # if the smallest distance is 0, we map all ranks of 0 distances to 0 as
      # well, while mapping the maximum non-NA rank to 1
      ranks <- ( (ranks - min) / (max - min) )
    } else {
      # if the smallest distance is larger than 0, than we just map the maximum
      # non-NA rank to 1 and let the smallest distance land whereever it will
      # (above 0)
      ranks <- ( ranks / max );
    }
  }

  # prune the NA distances and replace them with na.value
  ranks[na] <- na.value;
  return(ranks);
}
