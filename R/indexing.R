#' @title Compute the Index into a \code{\link[stats]{dist}} Object
#' @description Compute the index into a \code{\link[stats]{dist}} object based
#'   on two indexes of objects whose distance should be computed. See
#'   \url{http://stackoverflow.com/questions/9879608}.
#' @param i the first row or column index in the distance matrix
#' @param j the second row or column index in the distance matrix
#' @param n the number of rows/columns of the distance matrix
#' @return \code{0L} if \code{i == j}, else the index in the
#'   \code{\link[stats]{dist}} object
#' @export dist.index
dist.index <- function(i, j, n) { # given row, column, and n, return index
  if(i == j) { 0L
  } else if(i > j) {
    n*(j-1L) - ((j*(j-1L)) %/% 2L) + i - j
  } else {
    n*(i-1L) - ((i*(i-1L)) %/% 2L) + j - i
  }
}

#' @title Get the Number of Slots in a Distance Matrix
#' @description This function receives the number \code{n} of objects to compare
#'   and computes how many different distances will be stored in a distance
#'   matrix, i.e., what the number of slots of a \code{\link[stats]{dist}}
#'   object will be.
#' @param n the number of objects to compare
#' @return the required number of slots in a distance matrix
#' @export dist.slots
dist.slots <- function(n) {
  if(n > 1L) { (n * (n - 1L)) %/% 2L }
  else { 0L }
}

#' @title Compute the Indices of the Two Objects Corresponding to a
#'   \code{\link[stats]{dist}} Object Index
#' @description Compute the row and column indices of the two objects
#'   corresponding to a \code{\link[stats]{dist}} object index. See
#'   \url{http://stackoverflow.com/questions/9879608}.
#' @param index the index
#' @param n the number of rows/columns of the distance matrix
#' @return a vector with the two indices
#' @export dist.ij
dist.ij <- function(index,n) { # given index, return row and column
  nr = as.integer(ceiling(n - (1L + sqrt(1+4L*((n*n) - n - 2L*index))) / 2L));
  c(nr, n - (((2L*n - nr+1L)*nr) %/% 2L) + index + nr)
}

#' @title Compute the Distance between Two Objects based on a
#'   \code{\link[stats]{dist}} Object
#' @description Compute the index into a \code{\link[stats]{dist}} object based
#'   on two indexes of objects whose distance should be computed and return the
#'   corresponding distance.
#' @param distObj the instance of \code{\link[stats]{dist}}
#' @param i the first row or column index in the distance matrix
#' @param j the second row or column index in the distance matrix
#' @param n the number of rows/columns of the distance matrix
#' @return the distance between the objects at indices \code{i} and \code{j}
#' @export dist.get
dist.get <- function(distObj, i, j, n=attr(x=distObj, which="Size", exact=TRUE)){
  if(i == j) 0L else distObj[
    if(i > j) {
    n*(j-1L) - ((j*(j-1L))) %/% 2L + i - j
  } else {
    n*(i-1L) - ((i*(i-1L)) %/% 2L) + j - i
  }]
}


#' @title Generate a List of Index Pairs that can be used to Construct a
#'   \code{\link[stats]{dist}} Object
#' @description A list with the index pairs to be used to construct a distance
#'   matrix of \code{n} rows and columns is generated.
#' @param n the number of objects to compare
#' @return a list of object indexes that need to be compared, exactly in the
#'   same style and order as in \code{\link[stats]{dist}} objects
#' @export dist.indexes
dist.indexes <- function(n) {
  if(n>1L) unlist(lapply(X=seq_len(n-1L),
                         FUN=function(i) lapply(X=seq.int(from=(i+1L), to=n, by=1L),
                         FUN=function(j) c(i,j))), recursive = FALSE)
  else list()
}