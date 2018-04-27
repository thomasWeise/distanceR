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
  stopifnot(n > 1L, identical(length(distances), (n*(n-1L)) %/% 2L));

  attributes(distances) <- list(Size = n,
                                Labels = names,
                                Diag = FALSE,
                                Upper = FALSE,
                                method = "user");
  class(distances) <- "dist";
  distances <- force(distances);
  return(distances);
}
