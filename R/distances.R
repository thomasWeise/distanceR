#' @title A Function Computing the Euclidean Distance of Two Vectors
#' @description Well, a function computing the Euclidean distance between two
#'   vectors \code{a} and \code{b}.
#' @param a the first vector
#' @param b the second vector
#' @return the Euclidean distance between \code{a} and \code{b}
#' @export distance.euclidean
distance.euclidean <- function(a, b) {
  if(is.nan(a) || is.nan(b)) { return(NaN); }
  if(is.infinite(a) || is.infinite(b)) { return(+Inf); }
  sqrt(sum((a - b) ^ 2))
}
