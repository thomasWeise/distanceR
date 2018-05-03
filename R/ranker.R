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
#' @return an array with normalized distance ranks
#' @export rank.dist
rank.dist <- function(X) {
  # rank all the distances
  ranks <- rank(x=X, na.last=TRUE, ties.method="average");
  # find the number of NA distances
  n.na  <- sum(is.na(X));
  # get the range of distances
  range <- range(ranks, na.rm=TRUE);
  # the NA distances will always be at the end, so we can get the maximum non-NA
  # distance rank by subtracting n.na from the maximum rank
  max   <- range[2] - n.na;

  # get the minimum actual distance value and check if it is <= 0
  if(min(X, na.rm=TRUE) <= 0) {
    # if the smallest distance is 0, we map all ranks of 0 distances to 0 as
    # well, while mapping the maximum non-NA rank to 1
    min   <- range[1];
    return( (ranks - min) / (max - min) )
  } else {
    # if the smallest distance is larger than 0, than we just map the maximum
    # non-NA rank to 1 and let the smallest distance land whereever it will
    # (above 0)
    return( ranks / max );
  }
}
