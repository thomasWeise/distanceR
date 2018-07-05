#' @title Get a List of Node Pairs which are No Farther Away from Each Other
#'   than a Given Maximum Distance
#' @description A maximum distance \code{max.distance} is specified as well as a
#'   distance object \code{dist}. We return a list of binary vectors, where each
#'   vector identifies a connection between two nodes no longer than the
#'   specified maximum distance.
#' @param dist the distance object
#' @param max.distance the maximum distance for two nodes to be considered as
#'   connected
#' @return the list of all node-node connections
#' @include indexing.R
#' @export dist.connections
#' @examples
#' # create distance matrix with Euclidean distance between points in 1D plane
#' dm <- dist.create(dist.apply(X=c(1, 2, 4, 3, 10, 11)))
#' con <- dist.connections(dm, 1)
#' # [[1]]
#' # [1] 1 2 # -> points with coordinates "1" and "2" are connected
#' #
#' # [[2]]
#' # [1] 2 4 # -> points with coordinates "2" and "3" are connected
#' #
#' # [[3]]
#' # [1] 3 4 # -> points with coordinates "4" and "3" are connected
#' #
#' # [[4]]
#' # [1] 5 6 # -> points with coordinates "10" and "11" are connected
dist.connections <- function(dist, max.distance=1) {
  dist.indexes(dist.n.from.dm(dist))[dist <= max.distance]
}
