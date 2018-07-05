#' @title Create an instance of \code{\link[stats]{dist}}
#' @description Provide a distance matrix and names and then create a get an
#'   instance of \code{\link[stats]{dist}} in return.
#' @param distances the distance vector
#' @param names the vector of names or \code{NULL} if no names are needed
#' @return an instance \code{\link[stats]{dist}}
#' @export dist.create
#' @importFrom stats dist
#' @include indexing.R
dist.create <- function(distances, names=NULL) {
  distances <- force(distances);
  names <- force(names);


  if(is.null(names)) {
    # create nameless distance matrix
    n <- dist.n(length(distances));
    stopifnot(n >= 1L);
    attributes(distances) <- list(Size = n,
                                  Diag = FALSE,
                                  Upper = FALSE,
                                  method = "user");
  } else {
    # create a named distance matrix
    n <- length(names);
    stopifnot(n > 1L, identical(length(distances), dist.slots(n)));
    attributes(distances) <- list(Size = n,
                                  Labels = names,
                                  Diag = FALSE,
                                  Upper = FALSE,
                                  method = "user");
  }

  class(distances) <- "dist";
  distances <- force(distances);
  return(distances);
}
