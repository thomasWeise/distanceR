#'@title Merge Rows and Columns of a Distance Matrix According to a Selection
#'@description A list \code{selection} of vectors of indexes of rows/columns
#'  into a distance matrix is provided. A new distance matrix is created where
#'  only these selected elements remain and where the distances are
#'  \code{aggregates} of the original values.
#'
#'  Assume that a list \code{list(c(1,2), c(3,4))} is provided. This will create
#'  a distance matrix containing only a single distances, namely the one between
#'  the first and second group. This distance is then the \code{aggregate} of
#'  the original distances between \code{(1,3)}, \code{(1,4)}, \code{(2,3)}, and
#'  \code{(2,4)}. The aggregate is the \code{mean} by default, but could be any
#'  statistic.
#'
#'@param distObj the original distance object
#'@param selection the list of vectors selecting the matrix elements
#'@param names the new names to be used
#'@param aggregate the aggregate function to combine multiple distances
#'@param FUN.VALUE the distance to be used if there are no distances for a
#'  certain combination
#'@param cores the number of cores to be used for the computation
#'@return a new distance object representing the merged distances
#'@include indexing.R
#'@export dist.merge
#'@seealso dist.apply.samples
dist.merge <- function(distObj, selection,
                       names,
                       aggregate=mean, cores=1L, FUN.VALUE=+Inf) {
  dist.size <- .dist.size(distObj)
  stopifnot(dist.size  > 1L);

  dist.create(
    distances=dist.apply.samples(X=selection,
                                 FUN=function(i, j)
                                   dist.get(distObj=distObj,
                                            i=i,
                                            j=j,
                                            n=dist.size),
                                 aggregate=aggregate,
                                 FUN.VALUE=FUN.VALUE,
                                 cores=cores),
    names=names)
}
