#' @title Create an instance of \code{\link[stats]{dist}}
#' @description Provide a distance matrix and names and then create a get an
#'   instance of \code{\link[stats]{dist}} in return.
#' @param distances the distance vector
#' @param names the vector of names
#' @return an instance \code{\link[stats]{dist}}
#' @export dist.create
#' @importFrom stats dist
dist.create <- function(distances, names) {
  distances <- force(distances);
  names <- force(names);

  n <- length(names);
  if(n <= 1L) {
    return(stop("There must be at least two elements for creating a distance matrix."));
  }
  if(!(identical(length(distances), n*(n-1L) %/% 2L))) {
    return(stop("Lengths of names and distances do not match."));
  }

  attributes(distances) <- list(Size = n,
                                Labels = names,
                                Diag = FALSE,
                                Upper = FALSE,
                                method = "user");
  class(distances) <- "dist";
  distances <- force(distances);
  return(distances);
}
