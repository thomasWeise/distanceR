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
#'   object will be. The inverse function is \code{\link{dist.n}}.
#' @param n the number of objects to compare
#' @return the required number of slots in a distance matrix
#' @seealso dist.n
#' @export dist.slots
#' @examples
#' dist.slots(1)
#' # 0 ## (no distance between 1 object)
#' dist.slots(2)
#' # 1 ## (one distance between 2 objects)
#' dist.slots(3)
#' # 3 ## (AB, AC, BC)
#' dist.slots(4)
#' # 6 ## (AB, AC, AD, BC, BD, CD)
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

# get the size of a \code{\link[stats]{dist}} object
.dist.size <- function(distObj) attr(x=distObj, which="Size", exact=TRUE)

#' @title Compute the Number of Objects from the Number of Slots
#' @description Given the number of \code{slots} of a distance matric, get the
#'   number of objects whose distance is computed. This is the inverse function
#'   to \code{\link{dist.slots}}.
#' @param slots the number of slots in a distance matrix
#' @return the number of objects whose distances are stored in the matrix
#' @seealso dist.slots
#' @export dist.n
#' @examples
#' dist.slots(10)
#' # 45
#' dist.n(45)
#' # 10
dist.n <- function(slots) {
  if(slots >= 1L) (as.integer(sqrt((8L*slots) + 1L)) + 1L) %/% 2L
  else 1L
}

#' @title Get the Number of Objects that were used to Build a Distance Matrix
#' @description Given a distance matrix or plain vector \code{distObj}, find the
#'   number of objects whose distances can be represented by \code{distObj}.
#'   This function first tries to extract of \code{Size} attribute from the object.
#'   If that fails, it will use \code{\link{dist.n}} to compute the size based on
#'   the object's length.
#' @param distObj the distance object, matrix, or vector
#' @return the number of objects whose distance can be represented in this
#' @export dist.n.from.dm
#' @seealso dist.n
#' @examples
#' m <- matrix(rnorm(n=40), nrow=8)
#' d <- dist(m)
#' dist.n.from.dm(d)
#' # 8
dist.n.from.dm <- function(distObj) {
  s <- .dist.size(distObj);
  if(is.null(s)) dist.n(length(distObj))
  else as.integer(s)
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
dist.get <- function(distObj, i, j, n=.dist.size(distObj)){
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
